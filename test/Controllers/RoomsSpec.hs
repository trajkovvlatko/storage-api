module Controllers.RoomsSpec
  ( spec )

where

import Server ( app )
import Controllers.Rooms

import Test.Hspec as T hiding (shouldContain)
import Test.Hspec.Wai as W
import Test.Hspec.Wai.JSON
import Factories (createUser, createRoom)
import Helpers (loginUser, getToken, shouldContainString)
import Data.ByteString (unpack, pack)
import ClassyPrelude (IsString(fromString), MonadIO (liftIO))
import Network.HTTP.Types
import Test.Hspec.Wai (shouldRespondWith)
import Test.Hspec
import qualified Network.Wai.Test as WT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8

contentType = ("Content-Type", "application/x-www-form-urlencoded")

spec :: Spec
spec = with app $ do
  describe "index" $ do
    it "responds with 200" $ do
      get "/rooms" `shouldRespondWith` 200

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        let response = get "/rooms"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|]
        -- TODO:
        -- response `shouldRespondWith` 401

    context "with authenticated user" $ do
      it "returns an empty list for no rooms found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        let response = request "GET" "/rooms" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|]
        response `shouldRespondWith` 200

      it "returns a list of rooms for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        liftIO $ createRoom 0 "room0"
        loginResponse <- loginUser

        let response = request "GET" "/rooms" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{roomId}, name: #{roomName}}]|]
        response `shouldRespondWith` 200

  describe "preview" $ do
    it "responds with 200" $ do
      get "/rooms/1" `shouldRespondWith` 200

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId
        let response = get url
        response `shouldRespondWith` [json|{message: "Invalid user token."}|]
        -- TODO:
        -- response `shouldRespondWith` 401

    context "with authenticated user" $ do
      it "returns an empty list for no rooms found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        let url = fromString "/rooms/-1"
        request "GET" url [("token", getToken loginResponse)] "" `shouldRespondWith` [json|[]|]

      it "returns a room for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        liftIO $ createRoom 0 "room0"
        loginResponse <- loginUser

        let url = fromString $ "/rooms/" ++ show roomId
        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{roomId}, name: #{roomName}}|]
        response `shouldRespondWith` 200

  describe "create" $ do
    it "responds with 500 for missing parameter" $ do
      post "/rooms" "" `shouldRespondWith` 500

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        let postBody = "name=2323"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/rooms" headers postBody
        response `shouldRespondWith` [json|{message: "Invalid user token."}|]
        -- TODO:
        -- response `shouldRespondWith` 401

    context "with authenticated user" $ do
      it "creates a room" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "name=2323"
        let headers = [contentType, ("token", getToken loginResponse )]

        let response = request methodPost "/rooms" headers postBody
        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"2323\""
        response `shouldRespondWith` 200

  describe "update" $ do
    it "responds with 500 for missing parameter" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      (roomId, roomName) <- liftIO $ createRoom userId "room1"
      let url = fromString $ "/rooms/" ++ show roomId

      patch url "" `shouldRespondWith` 500

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let patchBody = "name=updated-name"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody
        response `shouldRespondWith` [json|{message: "Invalid user token."}|]
        -- TODO:
        -- response `shouldRespondWith` 401

    context "with authenticated user" $ do
      it "updates a room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let patchBody = "name=updated-name"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [contentType, ("token", getToken loginResponse )]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"updated-name\""
        response `shouldRespondWith` 200

  describe "delete" $ do
    context "without authenticated user" $ do

      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|]
        -- TODO:
        -- response `shouldRespondWith` 401

    context "with authenticated user" $ do
      it "deletes a room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [contentType, ("token", getToken loginResponse )]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"room1\""
        response `shouldRespondWith` 200
