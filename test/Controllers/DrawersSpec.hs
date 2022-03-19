module Controllers.DrawersSpec (spec) where

import ClassyPrelude (IsString (fromString), MonadIO (liftIO))
import Controllers.Drawers
import Factories (createDrawer, createRoom, createStorageUnit, createUser)
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
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"
        let response = request methodGet url [] ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no drawers found" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of drawers for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, level, drawerNote) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        liftIO $ createDrawer 0 0 1 "drawer0"
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{drawerId}, user_id: #{userId}, storage_unit_id: #{storageUnitId}, level: #{level}, note: #{drawerNote}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let url = fromString $ "/drawers/" ++ show drawerId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no drawers found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/drawers/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a drawer for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, level, drawerNote) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        liftIO $ createDrawer 0 0 1 "drawer0"
        loginResponse <- loginUser
        let url = fromString $ "/drawers/" ++ show drawerId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{drawerId}, user_id: #{userId}, storage_unit_id: #{storageUnitId}, level: #{level}, note: #{drawerNote}}|] {matchStatus = 200}

  describe "create" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let postBody = fromString "note=note1&level=123"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request methodPost url headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 500 for missing parameter" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let headers = [contentType, ("token", getToken loginResponse)]
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request methodPost url headers ""

        response `shouldRespondWith` 500

      it "creates a drawer" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        let postBody = fromString "note=note1&level=123"
        let headers = [contentType, ("token", getToken loginResponse)]
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request methodPost url headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        liftIO $ body `shouldContainString` "note\":\"note1\""
        liftIO $ body `shouldContainString` "level\":123"
        response `shouldRespondWith` 200

      it "does not create a drawer for other users storage unit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom otherUserId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit otherUserId roomId "storageUnit1"
        let postBody = fromString "note=note1&level=123"
        let headers = [contentType, ("token", getToken loginResponse)]
        let url = fromString $ "/storage_units/" ++ show storageUnitId ++ "/drawers"

        let response = request methodPost url headers postBody

        response `shouldRespondWith` 500

  describe "update" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let patchBody = "note=updated-note&level=123"
        let url = fromString $ "/drawers/" ++ show drawerId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "does not update record for missing parameters" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let url = fromString $ "/drawers/" ++ show drawerId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers ""
        body <- fmap WT.simpleBody response

        response `shouldRespondWith` 200
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        liftIO $ body `shouldContainString` "note\":\"drawer1\""
        liftIO $ body `shouldContainString` "level\":1"
        liftIO $ body `shouldContainString` fromString ("storage_unit_id\":" ++ show storageUnitId)

      it "updates a drawer" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let patchBody = "note=updated-note&level=123"
        let url = fromString $ "/drawers/" ++ show drawerId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "note\":\"updated-note\""
        liftIO $ body `shouldContainString` "level\":123"
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not update a drawer for other user's storage unit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (otherRoomId, _) <- liftIO $ createRoom otherUserId "room1"
        (otherStorageUnitId, _) <- liftIO $ createStorageUnit otherUserId otherRoomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer otherUserId otherStorageUnitId 1 "drawer1"
        let patchBody = "note=updated-note&level=123"
        let url = fromString $ "/drawers/" ++ show drawerId
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
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let url = fromString $ "/drawers/" ++ show drawerId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes a drawer" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (roomId, _) <- liftIO $ createRoom userId "room1"
        (storageUnitId, _) <- liftIO $ createStorageUnit userId roomId "storageUnit1"
        (drawerId, _, _) <- liftIO $ createDrawer userId storageUnitId 1 "drawer1"
        let url = fromString $ "/drawers/" ++ show drawerId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "note\":\"drawer1\""
        liftIO $ body `shouldContainString` "level\":1"
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not delete a drawer for other user's storage unit" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (otherRoomId, _) <- liftIO $ createRoom otherUserId "room1"
        (otherStorageUnitId, _) <- liftIO $ createStorageUnit otherUserId otherRoomId "storageUnit1"
        (otherDrawerId, _, _) <- liftIO $ createDrawer otherUserId otherStorageUnitId 1 "drawer1"
        let url = fromString $ "/drawers/" ++ show otherDrawerId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        -- TODO: Improve with a better response
        liftIO $ body `shouldBe` "[]"
