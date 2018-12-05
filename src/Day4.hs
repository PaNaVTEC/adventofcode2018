module Day4 where

import Text.Regex
import Data.List (sortOn, partition, splitAt, findIndices, delete, maximumBy)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Debug.Trace
import Data.Bifunctor

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day4.txt"
  print $ mostAsleepGuardInMinute . lines $ contents

mostAsleepGuardInMinute :: [String] -> Int
mostAsleepGuardInMinute lines =
  let shifts = groupByShift . mkEventsTable $ lines
      sleepyGuard = mostSleepyGuard shifts
      (Minute sleepyMinute) = mostSleepyMinute sleepyGuard shifts
  in sleepyGuard * sleepyMinute
--   trace
-- --  (show $ (filter shiftsForGuardId (groupByShift $ mkEventsTable lines)))
--   (show $ minutesSlept 10 $ (groupByShift $ mkEventsTable lines))
--   1

mostSleepyMinute :: GuardId -> [Shift] -> Minute
mostSleepyMinute guardId shifts = fst $ maximumBy foo . M.toList $ minutesSlept guardId shifts
  where
    foo a b = compare (snd a) (snd b)

minutesSlept :: GuardId -> [Shift] -> M.Map Minute Int
minutesSlept guardId shifts = foldl foo M.empty (filter shiftsForGuardId shifts)
  where
    shiftsForGuardId (Shift guardId' _) = guardId == guardId'
    foo m shift@(Shift guardId events) =
      fst (foldl foo2 (m, head events) (tail events))

    foo2 (asleep, (Minute prev, FallsAsleep)) e@(Minute now, WakesUp) =
      (foldr incrementMinute asleep [prev..now-1] ,e)
    foo2 (asleep, _) e = (asleep, e)

    incrementMinute min = M.alter (Just . maybe 1 (+1)) (Minute min)

mostSleepyGuard :: [Shift] -> GuardId
mostSleepyGuard shifts = fst $ maximumBy foo . M.toList $ minutesSleptByGuard shifts
  where foo a b = compare (snd a) (snd b)

minutesSleptByGuard :: [Shift] -> M.Map GuardId Int
minutesSleptByGuard = foldl foo M.empty
  where
    foo m shift@(Shift guardId events) =
      let mins = minutesSleptInShift events
      in M.alter (Just . maybe mins (+mins)) guardId m

minutesSleptInShift :: [(Minute, ShiftEvent)] -> Int
minutesSleptInShift [] = 0
minutesSleptInShift events = fst $ foldl foo (0, head events) (tail events)
  where
    foo (asleep, (Minute prev, FallsAsleep)) e@(Minute now, WakesUp) =
      (asleep + now - prev, e)
    foo (asleep, _) e = (asleep, e)

groupByShift :: [(QuasiDate, ShiftEvent)] -> [Shift]
groupByShift ll = catMaybes $ toShift <$> splitByIndices ll (beginShiftIndices ll)
  where
    beginShiftIndices = findIndices (isBeginShift . snd)
    toShift ((QuasiDate _ min, BeginsShift guardId) : rest) =
      Just $ Shift guardId (first onlyMinute <$> rest)
    toShift _ = Nothing
    onlyMinute (QuasiDate _ min) = min

isBeginShift :: ShiftEvent -> Bool
isBeginShift (BeginsShift _) = True
isBeginShift _ = False

mkEventsTable :: [String] -> [(QuasiDate, ShiftEvent)]
mkEventsTable lines' = sortOn fst . catMaybes $ (parse <$> lines')

parse :: String -> Maybe (QuasiDate, ShiftEvent)
parse str = (,)
  <$> (toQuasiDate =<< matchRegex dateRegex str)
  <*> (toEvent =<< matchRegex eventRegex str)
  where
    toQuasiDate [y, m, d, h, m'] = Just $ QuasiDate (y ++ m ++ d ++ h ++ m') (Minute $ read m')
    toQuasiDate _ = Nothing

    toEvent ["falls asleep", ""] = Just FallsAsleep
    toEvent ["wakes up", ""] = Just WakesUp
    toEvent [_, guard] = Just $ BeginsShift (read guard)
    toEvent _ = Nothing

dateRegex :: Regex
dateRegex = mkRegex "\\[([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})\\s([[:digit:]]{2}):([[:digit:]]{2})\\]\\s"

eventRegex :: Regex
eventRegex = mkRegex "(Guard #([[:digit:]]+) begins shift|falls asleep|wakes up)"

splitByIndices :: [a] -> [Int] -> [[a]]
splitByIndices xs is = go xs (zipWith subtract (0:is) is)
  where
    go [] _      = []
    go xs (i:is) = let (a, b) = splitAt i xs in a : go b is
    go xs _      = [xs]

data ShiftEvent = BeginsShift Int | FallsAsleep | WakesUp deriving (Eq, Show)
data QuasiDate = QuasiDate String Minute deriving (Eq, Show, Ord)
newtype Minute = Minute Int deriving (Eq, Show, Ord)
data GuardState = Awake | Asleep deriving (Eq, Show)
data Shift = Shift GuardId [(Minute, ShiftEvent)] deriving (Eq, Show)
type GuardId = Int
