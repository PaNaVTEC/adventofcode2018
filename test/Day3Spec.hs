{-# LANGUAGE OverloadedStrings #-}

module Day3Spec (main, spec) where

import           Test.Hspec
import Day3 (findOverlappingInches, findNonOverlappingClaim)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Overlaping squares" $ do

  it "finds number of overlaping areas" $
    findOverlappingInches [
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2"] `shouldBe` 4

  it "finds id of non overlapping claim" $
    findNonOverlappingClaim [
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2"] `shouldBe` "3"
