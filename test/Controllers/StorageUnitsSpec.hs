module Controllers.StorageUnitsSpec (spec) where

import ClassyPrelude (IsString (fromString), MonadIO (liftIO))
import Controllers.StorageUnits
import Factories (createRoom, createStorageUnit, createUser)
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
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/storage_units?room_id=" ++ show roomId
        let response = request methodGet url [] ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no storageUnits found" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        let url = fromString $ "/storage_units?room_id=" ++ show roomId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of storageUnits for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createStorageUnit otherUserId roomId "storageUnit0"
        let url = fromString $ "/storage_units?room_id=" ++ show roomId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{storageUnitId}, user_id: #{userId}, room_id: #{roomId}, name: #{storageUnitName}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no storageUnits found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/storage_units/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a storageUnit for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createStorageUnit otherUserId roomId "storageUnit0"
        loginResponse <- loginUser
        let url = fromString $ "/storage_units/" ++ show storageUnitId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{storageUnitId}, user_id: #{userId}, room_id: #{roomId}, name: #{storageUnitName}}|] {matchStatus = 200}

  describe "create" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        let postBody = fromString $ "name=2323&room_id=" ++ show roomId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/storage_units" headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 500 for missing parameter" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/storage_units" headers ""

        response `shouldRespondWith` 500

      it "creates a storage unit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        let postBody = fromString $ "name=2323&room_id=" ++ show roomId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/storage_units" headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"2323\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not create a storage unit for another user's room" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (otherRoomId, _) <- liftIO $ createRoom otherUserId "room2"

        let postBody = fromString $ "name=2323&room_id=" ++ show otherRoomId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/storage_units" headers postBody

        response `shouldRespondWith` 500

  describe "update" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let patchBody = fromString $ "name=updated-name&room_id" ++ show roomId
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "does not update record for missing parameters" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers ""
        body <- fmap WT.simpleBody response

        response `shouldRespondWith` 200
        liftIO $ body `shouldContainString` "name\":\"storageUnit1\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        liftIO $ body `shouldContainString` fromString ("room_id\":" ++ show roomId)

      it "updates a storageUnit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let patchBody = fromString $ "name=updated-name&room_id=" ++ show roomId
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "name\":\"updated-name\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not update storage units for other users rooms" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (otherRoomId, _) <- liftIO $ createRoom otherUserId "room2"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit otherUserId otherRoomId "storageUnit1"
        let patchBody = "name=updated-name"
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody

        body <- fmap WT.simpleBody response
        -- TODO: Improve with a better response
        liftIO $ body `shouldBe` "[]"

  describe "delete" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes a storage unit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"storageUnit1\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not delete storage units by other users" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom otherUserId "room1"
        (storageUnitId, storageUnitName) <- liftIO $ createStorageUnit otherUserId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        -- TODO: Improve with a better response
        liftIO $ body `shouldBe` "[]"
