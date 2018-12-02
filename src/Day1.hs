{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Text as T (splitOn, Text, strip, lines, intercalate)
import Data.Text.Read (signed, decimal)
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Map as M
import Control.Monad.State
import qualified Data.List as L
import Data.Maybe (fromMaybe)

mainPart1 :: IO ()
mainPart1 = do
  contents <- TIO.readFile "./input/day1.txt"
  print . frequencyChange . T.intercalate "," $ T.lines contents

frequencyChange :: T.Text -> Int
frequencyChange = foldl sumTexts 0 . T.splitOn ","

sumTexts :: Int -> T.Text -> Int
sumTexts b a = b + parse a

parse :: T.Text -> Int
parse a = either (const 0) fst (signed decimal $ T.strip a)

mainPart2 :: IO ()
mainPart2 = do
  contents <- TIO.readFile "./input/day1.txt"
  print . reachesTwiceFirst . T.intercalate "," $ T.lines contents

reachesTwiceFirst :: T.Text -> Int
reachesTwiceFirst input = go (0, M.empty)
  where
    go state =
      let (r, newState) = runState (calcState input) state
      in fromMaybe (go newState) r

frequencyAt :: T.Text -> Int -> Int
frequencyAt input freqIndex = sum (L.take freqIndex (infiniteFreqs input))

infiniteFreqs :: T.Text -> [Int]
infiniteFreqs input = cycle $ parse <$> T.splitOn "," input

calcState :: T.Text -> State (Int, M.Map Int Int) (Maybe Int)
calcState input = do
  (freqIndex, ss) <- get
  let
    newMap = M.alter updateOrInsert (frequencyAt input freqIndex) ss
    anyHasTwoOcc = L.find
      (\k -> (== Just 2) (M.lookup k newMap))
      (M.keys newMap)
  put (freqIndex + 1, newMap)
  return anyHasTwoOcc
  where
    updateOrInsert = Just . maybe 1 (+1)
