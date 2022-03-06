module Controllers.RoomsSpec
  ( spec )

where

import Server ( app )
import Controllers.Rooms

import Test.Hspec
import Test.Hspec.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec.Wai.JSON
import Factories (createUser, createRoom)
import Helpers (loginUser, getToken)

spec :: Spec
spec = with app $ do
  describe "index" $ do
    it "responds with 200" $ do
      get "/rooms" `shouldRespondWith` 200

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        get "/rooms" `shouldRespondWith` [json|{message: "Invalid user token."}|]

    context "with authenticated user" $ do
      it "returns an empty list for no rooms found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        request "GET" "/rooms" [("token", getToken loginResponse)] "" `shouldRespondWith` [json|[]|]

      it "returns a list of rooms for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        liftIO $ createRoom 0 "room0"
        loginResponse <- loginUser

        let response = request "GET" "/rooms" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{roomId}, name: #{roomName}}]|]
