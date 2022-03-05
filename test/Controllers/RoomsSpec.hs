{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.RoomsSpec
  ( spec )

where

import Server ( app )
import Controllers.Rooms

import Test.Hspec
import Test.Hspec.Wai as W
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with app $ do
  describe "index" $ do
    it "responds with 200" $ do
      get "/rooms" `shouldRespondWith` 200

    it "returns an error for missing token" $ do
      get "/rooms" `shouldRespondWith` [json|{message: "Invalid user token."}|]
