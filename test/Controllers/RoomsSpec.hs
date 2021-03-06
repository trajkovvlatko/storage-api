module Controllers.RoomsSpec (spec) where

import ClassyPrelude (IsString (fromString), MonadIO (liftIO))
import Controllers.Rooms
import Factories (createRoom, createUser)
import Helpers (getToken, loginUser, shouldContainString)
import Network.HTTP.Types
import qualified Network.Wai.Test as WT
import Server (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

contentType = ("Content-Type", "application/x-www-form-urlencoded")

spec :: Spec
spec = with app $ do
  describe "index" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        let response = get "/rooms"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no rooms found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        let response = request "GET" "/rooms" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of rooms for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createRoom otherUserId "room0"
        loginResponse <- loginUser

        let response = request "GET" "/rooms" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{roomId}, user_id: #{userId}, name: #{roomName}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no rooms found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/rooms/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a room for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createRoom otherUserId "room0"
        loginResponse <- loginUser
        let url = fromString $ "/rooms/" ++ show roomId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{roomId}, user_id: #{userId}, name: #{roomName}}|] {matchStatus = 200}

  describe "create" $ do
    it "responds with 500 for missing parameter" $ do
      post "/rooms" "" `shouldRespondWith` 500

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        let postBody = "name=2323"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/rooms" headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "creates a room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "name=2323"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/rooms" headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"2323\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
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

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "updates a room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let patchBody = "name=updated-name"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "name\":\"updated-name\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

  describe "delete" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes a room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, roomName) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/rooms/" ++ show roomId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"room1\""
        response `shouldRespondWith` 200
