module Day5 where

import Data.Char
import Debug.Trace
import Data.Function (fix)

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day5.txt"
  print $ length $ makePolymerReact contents

makePolymerReact :: String -> String
makePolymerReact = idempotently polymerIteration
  where
  polymerIteration = fst . foldl foo ("", (' ', ""))
  foo (acc, (lst, accNoAdd)) cchr =
     if react lst cchr
     then (accNoAdd, (' ', accNoAdd))
     else (acc ++ [cchr], (cchr, acc))

react :: Char -> Char -> Bool
react c c' =
  (toUpper c == toUpper c')
  && ((isUpper c && isLower c') || (isLower c && isUpper c'))

idempotently :: Eq a => (a -> a) -> a -> a
idempotently = fix $ \i f a ->
  let a' = f a
  in if a' == a then a else i f a'
