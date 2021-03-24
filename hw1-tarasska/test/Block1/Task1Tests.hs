module Block1.Task1Tests
  ( daysOfTheWeekTestTree
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task1 (DaysOfTheWeek (..), afterDays, daysToParty, isWeekend, nextDay)

daysOfTheWeekTestTree :: IO TestTree
daysOfTheWeekTestTree = testSpec "Days of the Week" daysOfTheWeekSpec

daysOfTheWeekSpec :: Spec
daysOfTheWeekSpec = do
  describe "nextDay" $ do
    it "nextDay Monday"    $ nextDay Monday    `shouldBe` Tuesday
    it "nextDat Tuesday"   $ nextDay Tuesday   `shouldBe` Wednesday
    it "nextDat Wednesday" $ nextDay Wednesday `shouldBe` Thursday
    it "nextDat Thursday"  $ nextDay Thursday  `shouldBe` Friday
    it "nextDat Friday"    $ nextDay Friday    `shouldBe` Saturday
    it "nextDat Saturday"  $ nextDay Saturday  `shouldBe` Sunday
    it "nextDat Sunday"    $ nextDay Sunday    `shouldBe` Monday

  describe "afterDays" $ do
    it "afterDays Monday 0"     $ afterDays Monday     0 `shouldBe` Monday
    it "afterDays Tuesday 1"    $ afterDays Tuesday    1 `shouldBe` Wednesday
    it "afterDays Wednesday 23" $ afterDays Wednesday 23 `shouldBe` Friday
    it "afterDays Thursday 3"   $ afterDays Thursday   3 `shouldBe` Sunday
    it "afterDays Friday 21"    $ afterDays Friday    21 `shouldBe` Friday
    it "afterDays Saturday 7"   $ afterDays Saturday   7 `shouldBe` Saturday
    it "afterDays Sunday 6"     $ afterDays Sunday     6 `shouldBe` Saturday

  describe "isWeekend" $ do
    it "isWeekend Monday"    $ isWeekend Monday    `shouldBe` False
    it "isWeekend Tuesday"   $ isWeekend Tuesday   `shouldBe` False
    it "isWeekend Wednesday" $ isWeekend Wednesday `shouldBe` False
    it "isWeekend Thursday"  $ isWeekend Thursday  `shouldBe` False
    it "isWeekend Friday"    $ isWeekend Friday    `shouldBe` False
    it "isWeekend Saturday"  $ isWeekend Saturday  `shouldBe` True
    it "isWeekend Sunday"    $ isWeekend Sunday    `shouldBe` True

  describe "daysToParty" $ do
    it "daysToParty Monday"    $ daysToParty Monday    `shouldBe` 4
    it "daysToParty Tuesday"   $ daysToParty Tuesday   `shouldBe` 3
    it "daysToParty Wednesday" $ daysToParty Wednesday `shouldBe` 2
    it "daysToParty Thursday"  $ daysToParty Thursday  `shouldBe` 1
    it "daysToParty Friday"    $ daysToParty Friday    `shouldBe` 0
    it "daysToParty Saturday"  $ daysToParty Saturday  `shouldBe` 6
    it "daysToParty Sunday"    $ daysToParty Sunday    `shouldBe` 5
