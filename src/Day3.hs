{-# language OverloadedStrings #-}

module Day3 where

import Text.Regex
import Data.Maybe (catMaybes, maybe)
import Data.List (find)
import qualified Data.Map as M

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day3.txt"
  print $ findOverlappingInches . lines $ contents

findOverlappingInches :: [String] -> Int
findOverlappingInches sqlines = intersectingInches $ catMaybes (toSquare <$> sqlines)

intersectingInches :: [Square] -> Int
intersectingInches squares = length . M.filter (>1) $ overlappedPoints squares

overlappedPoints :: [Square] -> M.Map Point Int
overlappedPoints squares = foldr countOverlappedAreas M.empty (pointsOf =<< squares)
  where
    countOverlappedAreas = M.alter (Just . maybe 1 (+1))

pointsOf :: Square -> [Point]
pointsOf square = [(x', y') | x' <- [(left square)..(width square + left square - 1)]
                            , y' <- [(top square)..(height square + top square - 1)]]

toSquare :: String -> Maybe Square
toSquare sqstr = toSquare' =<< matchRegex squareRegex sqstr
  where
    toSquare' :: [String] -> Maybe Square
    toSquare' [i, l, t, w, h] = Just $ Square i (read l) (read t) (read w) (read h)
    toSquare' _ = Nothing

squareRegex :: Regex
squareRegex = mkRegex "#\\s*([[:digit:]]+)\\s*@\\s*([[:digit:]]+),([[:digit:]]+):\\s*([[:digit:]]+)x([[:digit:]]+)"

right :: Square -> Int
right sq = left sq + width sq

bottom :: Square -> Int
bottom sq = top sq + height sq

data Square = Square {
  claimid :: String, left :: Int, top :: Int, width :: Int, height :: Int
} deriving (Show, Eq)
type Point = (Int, Int)

mainPart2 :: IO ()
mainPart2 = do
  contents <- readFile "./input/day3.txt"
  print $ findNonOverlappingClaim . lines $ contents

findNonOverlappingClaim :: [String] -> String
findNonOverlappingClaim sqlines =
  let squares = (\s -> (s, pointsOf s)) <$> catMaybes (toSquare <$> sqlines)
      pointsWith1area = M.filter (==1) . overlappedPoints $ fst <$> squares
  in maybe
     "Not found"
     (claimid . fst)
     (find (pointsOnlyAppearOne pointsWith1area) squares)
  where
    pointsOnlyAppearOne :: M.Map Point Int -> (Square, [Point]) -> Bool
    pointsOnlyAppearOne p (_, points) = length points == length (M.filterWithKey (\k _ -> k `elem` points) p)
