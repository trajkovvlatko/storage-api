module Controllers.ItemTypesSpec (spec) where

import ClassyPrelude (IsString (fromString), MonadIO (liftIO))
import Controllers.ItemTypes
import Factories (createItemType, createUser)
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
        let response = get "/item_types"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no item_types found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        let response = request "GET" "/item_types" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of item_types" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"

        let response = request "GET" "/item_types" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{itemTypeId}, label: #{itemTypeLabel}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let url = fromString $ "/item_types/" ++ show itemTypeId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no item_types found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/item_types/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns an itemType" $ do
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        liftIO $ createItemType "itemType0"
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString $ "/item_types/" ++ show itemTypeId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{itemTypeId}, label: #{itemTypeLabel}}|] {matchStatus = 200}

  describe "create" $ do
    it "responds with 500 for missing parameter" $ do
      post "/item_types" "" `shouldRespondWith` 500

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        let postBody = "label=2323"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/item_types" headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 200" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "label=item-type-label"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/item_types" headers postBody

        response `shouldRespondWith` 200

      it "creates an itemType" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "label=item-type-label"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/item_types" headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "label\":\"item-type-label\""

  describe "update" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let patchBody = "label=updated-label"
        let url = fromString $ "/item_types/" ++ show itemTypeId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 200 for missing parameter" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let headers = [contentType, ("token", getToken loginResponse)]
        let url = fromString $ "/item_types/" ++ show itemTypeId

        let response = request methodPatch url headers ""

        response `shouldRespondWith` 200

      it "updates a itemType" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let patchBody = "label=updated-label"
        let url = fromString $ "/item_types/" ++ show itemTypeId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "label\":\"updated-label\""
        response `shouldRespondWith` 200

  describe "delete" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let url = fromString $ "/item_types/" ++ show itemTypeId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes a itemType" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemTypeId, itemTypeLabel) <- liftIO $ createItemType "itemType1"
        let url = fromString $ "/item_types/" ++ show itemTypeId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "label\":\"itemType1\""
        response `shouldRespondWith` 200
