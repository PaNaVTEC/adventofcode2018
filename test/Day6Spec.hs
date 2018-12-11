module Day6Spec (main, spec) where

import           Test.Hspec
import Day6
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Areas" $ do

  it "largest non-infinite area" $
    largestNonInfiniteArea [
      "1, 1",
      "1, 6",
      "8, 3",
      "3, 4",
      "5, 5",
      "8, 9"] `shouldBe` 17

  it "Generates a grid to calculate distances" $
    let a = (1, 1)
        b = (1, 6)
        c = (8, 3)
        d = (3, 4)
        e = (5, 5)
        f = (8, 9)
    in mkGrid [a, b, c, d, e, f]
      `shouldBe`
    [
      Just (a,2),Just (a,1),Just (a,2),Just (a,3),Just (a,4),Nothing,Just (c,5),Just (c,4),Just (c,3),Just (c,4),
      Just (a,1),Just (a,0),Just (a,1),Just (a,2),Just (a,3),Nothing,Just (c,4),Just (c,3),Just (c,2),Just (c,3),
      Just (a,2),Just (a,1),Just (a,2),Just (d,2),Just (d,3),Just (e,3),Just (c,3),Just (c,2),Just (c,1),Just (c,2),
      Just (a,3),Just (a,2),Just (d,2),Just (d,1),Just (d,2),Just (e,2),Just (c,2),Just (c,1),Just (c,0),Just (c,1),
      Nothing,Nothing,Just (d,1),Just (d,0),Just (d,1),Just (e,1),Just (e,2),Just (c,2),Just (c,1),Just (c,2),
      Just (b,2),Just (b,1),Nothing,Just (d,1),Just (e,1),Just (e,0),Just (e,1),Just (e,2),Just (c,2),Just (c,3),
      Just (b,1),Just (b,0),Just (b,1),Nothing,Just (e,2),Just (e,1),Just (e,2),Just (e,3),Nothing,Nothing,
      Just (b,2),Just (b,1),Just (b,2),Nothing,Just (e,3),Just (e,2),Just (e,3),Just (f,3),Just (f,2),Just (f,3),
      Just (b,3),Just (b,2),Just (b,3),Nothing,Just (e,4),Just (e,3),Just (f,3),Just (f,2),Just (f,1),Just (f,2),
      Just (b,4),Just (b,3),Just (b,4),Nothing,Just (f,4),Just (f,3),Just (f,2),Just (f,1),Just (f,0),Just (f,1)
    ]

  it "calcualtes length of the safest area" $
    safeRegionSize 32 [
      "1, 1",
      "1, 6",
      "8, 3",
      "3, 4",
      "5, 5",
      "8, 9"] `shouldBe` 16
