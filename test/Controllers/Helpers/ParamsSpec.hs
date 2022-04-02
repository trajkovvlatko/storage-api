module Controllers.Helpers.ParamsSpec (spec) where

import Controllers.Helpers.Params (clearString)
import Server (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = do
  describe "clearString" $ do
    it "returns an empty string for empty string input" $ do
      clearString "" `shouldBe` ""
      clearString "    " `shouldBe` ""

    it "returns an empty string for input with only special characters" $ do
      clearString "!@$%^&" `shouldBe` ""

    it "returns an empty string for input with only numbers" $ do
      clearString "1234" `shouldBe` ""

    it "removes special characters and numbers from string" $ do
      clearString "! aaa @ bbb $% 1 23ccc4^&" `shouldBe` "aaa bbb ccc"

    it "lowers capital letters" $ do
      clearString "! BBB @ aaa $% 1 23 ccc 4^& DDD " `shouldBe` "bbb aaa ccc ddd"

    it "removes duplicate empty spaces" $ do
      clearString "  aaa   bbb c " `shouldBe` "aaa bbb c"
