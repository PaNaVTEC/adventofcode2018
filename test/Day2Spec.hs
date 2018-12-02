{-# LANGUAGE OverloadedStrings #-}

module Day2Spec (main, spec) where

import           Test.Hspec
import Day2 (countRepeatedChars, findBoxesThatDiffersByAChar)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Checksum" $ do

  it "Count repetitions" $ do
    countRepeatedChars "abcdef" `shouldBe` (False, False)
    countRepeatedChars "bababc" `shouldBe` (True,  True)
    countRepeatedChars "abbcde" `shouldBe` (True,  False)
    countRepeatedChars "abcccd" `shouldBe` (False, True)
    countRepeatedChars "aabcdd" `shouldBe` (True,  False)
    countRepeatedChars "abcdee" `shouldBe` (True,  False)
    countRepeatedChars "ababab" `shouldBe` (False, True)

  it "find boxes that differs by one character" $
    findBoxesThatDiffersByAChar
      ["abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"] `shouldBe` Just "fgij"
