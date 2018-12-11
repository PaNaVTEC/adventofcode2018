module Day7Spec (main, spec) where

import           Test.Hspec
import Day7
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Order" $ do

  it "calculates the correct order" $
    calculateOrder [
      "Step C must be finished before step A can begin.",
      "Step C must be finished before step F can begin.",
      "Step A must be finished before step B can begin.",
      "Step A must be finished before step D can begin.",
      "Step B must be finished before step E can begin.",
      "Step D must be finished before step E can begin.",
      "Step F must be finished before step E can begin."] `shouldBe` "CABDFE"
