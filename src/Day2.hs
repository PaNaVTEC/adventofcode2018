module Day2 (countRepeatedChars, findBoxesThatDiffersByAChar) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T (splitOn, Text, strip, lines, intercalate)
import Data.Bool
import Data.Maybe
import Debug.Trace

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day2.txt"
  let (twos, threes) = countBoolsTuple (countRepeatedChars <$> lines contents)
  print $ twos * threes

countBoolsTuple :: [(Bool, Bool)] -> (Int, Int)
countBoolsTuple = foldl sumBools (0, 0)
  where
    sumBools (twos, threes) (a, b) = (twos + boolToInt a, threes + boolToInt b)

countRepeatedChars :: String -> (Bool, Bool)
countRepeatedChars = toTuple . countRepetitions
  where
    countRepetitions = foldr (M.alter (Just . maybe 1 (+1))) M.empty

    toTuple m = let twosAndThrees = countTwosAndThrees . (M.!) m <$> M.keys m
                in (any ((==True) . fst) twosAndThrees,
                    any ((==True) . snd) twosAndThrees)

    countTwosAndThrees 2 = (True, False)
    countTwosAndThrees 3 = (False, True)
    countTwosAndThrees _ = (False, False)

mainPart2 :: IO ()
mainPart2 = do
  contents <- readFile "./input/day2.txt"
  print $ findBoxesThatDiffersByAChar $ lines contents

findBoxesThatDiffersByAChar :: [String] -> Maybe String
findBoxesThatDiffersByAChar listOfWords = listToMaybe . join $ wordsDifferByCharAtPosition' <$> listOfWords
  where
    wordsDifferByCharAtPosition' word = catMaybes $ wordsDifferByCharAtPosition word <$> L.delete word listOfWords

wordsDifferByCharAtPosition :: String -> String -> Maybe String
wordsDifferByCharAtPosition word1 word2 = case foldl incIfNotEqual 0 (zip word1 word2) of
  1 -> Just $ L.delete (head $ word1 L.\\ word2) word1
  _ -> Nothing
  where
    incIfNotEqual b (c1, c2) = b + boolToInt (c1 /= c2)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0
