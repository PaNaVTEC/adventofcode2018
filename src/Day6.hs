module Day6 where

import Data.Function (on)
import Data.List (break, minimumBy, maximumBy, group, sort, nub, (!!), notElem)
import Debug.Trace
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Fixed (mod')

mainPart1 :: IO ()
mainPart1 = do
  contents <- readFile "./input/day6.txt"
  print $ largestNonInfiniteArea . lines $ contents

largestNonInfiniteArea :: [String] -> Int
largestNonInfiniteArea lines' =
  let coords = parseCoordinates lines'
      grid = mkGrid coords
      infiniteAreas' = infiniteAreas (calcBoardBounds coords) grid
      nonInfiniteCoords = fst <$> filter ((`notElem` infiniteAreas') . fst) (catMaybes grid)
      occupiedAreas = length <$> (group . sort) nonInfiniteCoords
  in maximum occupiedAreas

parseCoordinates :: Functor f => f String -> f Coordinate
parseCoordinates lines' = toCoordinate <$> lines'

infiniteAreas :: BoardBounds -> [Maybe Point] -> [Coordinate]
infiniteAreas bb points = fst <$> catMaybes ((!!) points <$> edges bb)

edges :: BoardBounds -> [Int]
edges bb = let w' = width bb
               h' = height bb
               bottomRight = w' * h' -1
               bottomLeft = bottomRight - w' + 1
               topLeft = 0
               topRight = w' -1
               top = [topLeft..topRight]
               bottom = [bottomLeft..bottomRight]
               left = [topLeft,w'..bottomLeft]
               right = [topRight,(topRight + w')..bottomRight]
           in nub $ top ++ bottom ++ left ++ right

notSurrounded :: [Coordinate] -> [Coordinate]
notSurrounded coords =
  let minC' = minCoord coords
      maxC' = maxCoord coords
  in filter (notInEdge minC' maxC') coords
  where
    notInEdge (mx, my) (xx, xy) (x, y) = mx /= x && xx /= x && my /= y && xy /= y

calcBoardBounds :: [Coordinate] -> BoardBounds
calcBoardBounds coords = let (maxX, maxY) = maxCoord coords
                             edge = max maxX maxY + 1
                         in BoardBounds edge edge

mkGrid :: [Coordinate] -> [Maybe Point]
mkGrid coords = let boardBounds = calcBoardBounds coords
                in calculateNearestArea boardBounds <$> [0..(width boardBounds * height boardBounds - 1)]
  where
    calculateNearestArea :: BoardBounds -> Int -> Maybe Point
    calculateNearestArea boardBounds fixedPos =
      let coord = fromFixedPos fixedPos boardBounds
          distances = (\i -> (i, manhattanDistance coord i)) <$> coords
          point@(_, minDistance) = minimumBy (compare `on` snd) distances
      in bool
        (Just point)
        Nothing
        ((/= 1) . length . filter ((==minDistance) . snd) $ distances)

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

maxCoord :: [Coordinate] -> Coordinate
maxCoord points = (
      fst . maximumBy (compare `on` fst) $ points,
      snd . maximumBy (compare `on` snd) $ points)

minCoord :: [Coordinate] -> Coordinate
minCoord points = (
      fst . minimumBy (compare `on` fst) $ points,
      snd . minimumBy (compare `on` snd) $ points)

toCoordinate :: String -> Coordinate
toCoordinate s = let (xs, ys) = break (== ',') s
            in (read xs, (read . tail) ys)

toFixedPos :: Coordinate -> BoardBounds -> Int
toFixedPos (x, y) b = y * width b + x

fromFixedPos :: Int -> BoardBounds -> Coordinate
fromFixedPos fixedPos bounds = (
  fromIntegral fixedPos `mod'` fromIntegral (width bounds),
  fromIntegral fixedPos `div` fromIntegral (height bounds)
  )

type Coordinate = (Int, Int)
type ClosestArea = Coordinate
type Distance = Int
type Point = (ClosestArea, Distance)
data BoardBounds = BoardBounds { width :: Int, height :: Int } deriving (Eq, Show)
