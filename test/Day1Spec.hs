{-# LANGUAGE OverloadedStrings #-}

module Day1Spec (main, spec) where

import           Test.Hspec
import Day1 (frequencyChange, reachesTwiceFirst)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Frequency changes" $ do

  it "substracts and adds" $ do
    frequencyChange "-1, -2, -3" `shouldBe` -6
    frequencyChange "+1, +1, +1" `shouldBe` 3
    frequencyChange "+1, +1, -2" `shouldBe` 0

  it "reaches twice the frequency first" $ do
    reachesTwiceFirst "+1, -1" `shouldBe` 0
    reachesTwiceFirst "+3, +3, +4, -2, -4" `shouldBe` 10
    reachesTwiceFirst "-6, +3, +8, +5, -6" `shouldBe` 5
    reachesTwiceFirst "+7, +7, -2, -7, -4" `shouldBe` 14
    reachesTwiceFirst "+1, -2, +3, +1, +1, -2" `shouldBe` 2
