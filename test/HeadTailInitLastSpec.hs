module HeadTailInitLastSpec (main, spec) where

import Test.Hspec

import HeadTailInitLast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "middle" $ do
        it "returns all but the first and last items" $ do
            middle "abc" `shouldBe` "b"
            middle "abcd" `shouldBe` "bc"
            middle "abcde" `shouldBe` "bcd"
        it "returns an empty list with fewer than three items" $ do
            middle "" `shouldBe` ""
            middle "a" `shouldBe` ""
            middle "ab" `shouldBe` ""
    describe "secondToLast" $ do
        it "returns the second to last number" $ do
            secondToLast [1, 2] `shouldBe` 1
            secondToLast [1, 2, 3] `shouldBe` 2
            secondToLast [1, 2, 3, 4] `shouldBe` 3
        it "returns zero with fewer than two items" $ do
            secondToLast [] `shouldBe` 0
            secondToLast [1] `shouldBe` 0
    describe "ends" $ do
        it "returns the first and last items" $ do
            ends "a" `shouldBe` "aa"
            ends "ab" `shouldBe` "ab"
            ends "abc" `shouldBe` "ac"
            ends "abcd" `shouldBe` "ad"
        it "returns an empty list from an empty list" $ do
            ends "" `shouldBe` ""
    describe "firstTwo" $ do
        it "returns the first two items" $ do
            firstTwo "ab" `shouldBe` "ab"
            firstTwo "abc" `shouldBe` "ab"
            firstTwo "abcd" `shouldBe` "ab"
        it "returns the first item with only one item" $ do
            firstTwo "a" `shouldBe` "a"
        it "returns an empty list from an empty list" $ do
            firstTwo "" `shouldBe` ""
