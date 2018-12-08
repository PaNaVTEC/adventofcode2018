{-# LANGUAGE OverloadedStrings #-}

module Day4Spec (main, spec) where

import           Test.Hspec
import Day4 (mostAsleepGuardInMinute, guardSleptInTheSameMinuteMost)

main :: IO ()
main = hspec spec

input = [
    "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-01 00:25] wakes up",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-02 00:50] wakes up",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-05 00:55] wakes up"]

spec :: Spec
spec = describe "Guard shifts" $ do

  it "Most asleep guard in a specific minute" $
    mostAsleepGuardInMinute input `shouldBe` 10 * 24

  it "which guard is most frequently asleep on the same minute" $
    guardSleptInTheSameMinuteMost input `shouldBe` 99 * 45
