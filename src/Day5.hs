module Day5 where

import Data.Char (toUpper, isUpper, isLower)
import Data.Function (fix)
import Data.Bool (bool)
import Data.List (minimum)

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day5.txt"
  print $ length $ makePolymerReact (trim contents)

trim :: String -> String
trim = unwords . words

makePolymerReact :: String -> String
makePolymerReact = idempotently polymerIteration
  where
  polymerIteration = reverse . fst . foldl foo ("", (' ', ""))
  foo (acc, (lst, accNoAdd)) cchr = bool
    (cchr : acc, (cchr, acc))
    (accNoAdd, (' ', accNoAdd))
    (react lst cchr)

react :: Char -> Char -> Bool
react c c' = (toUpper c == toUpper c')
  && ((isUpper c && isLower c') || (isLower c && isUpper c'))

idempotently :: Eq a => (a -> a) -> a -> a
idempotently = fix $ \i f a ->
  let a' = f a
  in if a' == a then a else i f a'

mainPart2 :: IO ()
mainPart2 = do
  contents <- readFile "./input/day5.txt"
  print $ shorterPolymer (trim contents)

shorterPolymer :: String -> Int
shorterPolymer contents = minimum $ polymerLengthWithout <$> ['A'..'Z']
  where
    polymerLengthWithout polymer = length . makePolymerReact $ filter ((/= polymer). toUpper) contents
