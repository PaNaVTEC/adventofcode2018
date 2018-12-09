module Day5Spec (main, spec) where

import           Test.Hspec
import Day5

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Polymers" $ do

  it "lowercase Uppercase reacts, the rest remains" $ do
    makePolymerReact  "cC" `shouldBe` ""
    makePolymerReact  "acCA" `shouldBe` ""
    makePolymerReact  "dabAcCaCBAcCcaDA" `shouldBe` "dabCBAcaDA"
