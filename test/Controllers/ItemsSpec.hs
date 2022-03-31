module Controllers.ItemsSpec (spec) where

import ClassyPrelude (IsString (fromString), LazySequence (fromStrict), MonadIO (liftIO))
import Controllers.Items
import Factories (createColor, createDrawer, createItem, createItemType, createRoom, createStorageUnit, createUser)
import Helpers (getToken, loginUser, shouldContainString)
import Lib.Auth (UserId)
import Models.Color (ColorId)
import Models.Drawer (DrawerId)
import Models.ItemType (ItemTypeId)
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
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let url = fromString $ "/items?drawer_id=" ++ show drawerId
        let response = request methodGet url [] ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no items found" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (drawerId, _, _, _) <- liftIO $ createDrawer userId 1 "drawer1"
        let url = fromString $ "/items?drawer_id=" ++ show drawerId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of items for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemId, userId, drawerId, colorId, itemTypeId, itemName) <- liftIO $ createItem userId "red" "electronics" "item name"

        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createItem otherUserId "blue" "medicine" "item name 2"

        let url = fromString $ "/items?drawer_id=" ++ show drawerId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{itemId}, user_id: #{userId}, drawer_id: #{drawerId}, color_id: #{colorId}, item_type_id: #{itemTypeId}, name: #{itemName}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (itemId, _, _, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let url = fromString $ "/items/" ++ show itemId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no items found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/items/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns an item for a user" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (itemId, userId, drawerId, colorId, itemTypeId, itemName) <- liftIO $ createItem userId "red" "electronics" "item name"

        otherUserId <- liftIO $ createUser "other@other.com" "password"
        liftIO $ createItem userId "blue" "medicine" "item name 2"

        loginResponse <- loginUser
        let url = fromString $ "/items/" ++ show itemId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{itemId}, user_id: #{userId}, drawer_id: #{drawerId}, color_id: #{colorId}, item_type_id: #{itemTypeId}, name: #{itemName}}|] {matchStatus = 200}

  describe "create" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (drawerId, _, _, _) <- liftIO $ createDrawer userId 1 "drawer1"
        (colorId, _) <- liftIO $ createColor "red"
        (itemTypeId, _) <- liftIO $ createItemType "electronics"
        let postBody = fromString $ "color_id=" ++ show colorId ++ "&item_type_id=" ++ show itemTypeId ++ "&drawer_id=" ++ show drawerId ++ "&name=item-name"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/items" headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 500 for missing parameter" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/items" headers ""

        response `shouldRespondWith` 500

      it "creates an item" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (drawerId, _, _, _) <- liftIO $ createDrawer userId 1 "drawer1"
        (colorId, _) <- liftIO $ createColor "red"
        (itemTypeId, _) <- liftIO $ createItemType "electronics"
        let postBody = fromString $ "color_id=" ++ show colorId ++ "&item_type_id=" ++ show itemTypeId ++ "&drawer_id=" ++ show drawerId ++ "&name=item-name"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/items" headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        liftIO $ body `shouldContainString` fromString ("drawer_id\":" ++ show drawerId)
        liftIO $ body `shouldContainString` fromString ("color_id\":" ++ show colorId)
        liftIO $ body `shouldContainString` fromString ("item_type_id\":" ++ show itemTypeId)
        liftIO $ body `shouldContainString` "name\":\"item-name"
        response `shouldRespondWith` 200

      it "does not create an item for other users drawers" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (drawerId, _, _, _) <- liftIO $ createDrawer otherUserId 1 "drawer1"
        (colorId, _) <- liftIO $ createColor "red"
        (itemTypeId, _) <- liftIO $ createItemType "electronics"
        let postBody = fromString $ "color_id=" ++ show colorId ++ "&item_type_id=" ++ show itemTypeId ++ "&drawer_id=" ++ show drawerId ++ "&name=item-name"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/items" headers postBody

        response `shouldRespondWith` 500

  describe "update" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let patchBody = fromString $ "name=updated-name&drawer_id=" ++ show drawerId
        let url = fromString $ "/items/" ++ show itemId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "does not update record for missing parameters" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let url = fromString $ "/items/" ++ show itemId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers ""
        body <- fmap WT.simpleBody response

        response `shouldRespondWith` 200
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        liftIO $ body `shouldContainString` "name\":\"item name\""
        liftIO $ body `shouldContainString` fromString ("drawer_id\":" ++ show drawerId)

      it "updates an item" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let patchBody = fromString $ "name=updated-name&drawer_id=" ++ show drawerId
        let url = fromString $ "/items/" ++ show itemId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "name\":\"updated-name\""
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not update an item for other user's drawers" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem otherUserId "red" "electronics" "item name"
        let patchBody = "name=updated-name"
        let url = fromString $ "/items/" ++ show itemId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody

        body <- fmap WT.simpleBody response
        -- TODO: Improve with a better response
        liftIO $ body `shouldBe` "[]"

  describe "delete" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let url = fromString $ "/items/" ++ show itemId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes an item" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem userId "red" "electronics" "item name"
        let url = fromString $ "/items/" ++ show itemId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "name\":\"item name\""
        liftIO $ body `shouldContainString` fromString ("id\":" ++ show itemId)
        liftIO $ body `shouldContainString` fromString ("user_id\":" ++ show userId)
        response `shouldRespondWith` 200

      it "does not delete an item for other user's drawers" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        otherUserId <- liftIO $ createUser "other@other.com" "password"
        loginResponse <- loginUser
        (itemId, _, drawerId, _, _, _) <- liftIO $ createItem otherUserId "red" "electronics" "item name"
        let url = fromString $ "/items/" ++ show itemId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        -- TODO: Improve with a better response
        liftIO $ body `shouldBe` "[]"
