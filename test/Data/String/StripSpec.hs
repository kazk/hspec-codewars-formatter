module Data.String.StripSpec
    (
      main
    , spec
    ) where

import Test.Hspec
import Test.QuickCheck

import Data.String.Strip

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "strip" $ do
        it "removes leading and trailing whitespace" $ do
            strip "\t  foo bar\n" `shouldBe` "foo bar"
        it "is idempotent" $ property $
            \str -> strip str === strip (strip str)
    describe "stripDigits" $ do
        it "removes leading and trailing digits" $ do
            stripDigits "00foo bar00" `shouldBe` "foo bar"
        it "removes only leading and trailing digits" $ do
            stripDigits "a00faa bar0f0" `shouldBe` "a00faa bar0f"
