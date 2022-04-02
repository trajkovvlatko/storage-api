module Controllers.SearchSpec (spec) where

import Controllers.Search
import Factories (createItem, createUser)
import Helpers (getToken, loginUser, shouldContainString)
import Server (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with app $ do
  describe "index" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        liftIO $ createItem userId "black" "electronics" "laptop"

        let response = get "/search/basic/laptop"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

  context "with authenticated user" $ do
    it "returns an empty list for missing parameter" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      liftIO $ createItem userId "black" "electronics" "laptop"
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/" [("token", getToken loginResponse)] ""

      response `shouldRespondWith` [json|[]|] {matchStatus = 200}

    it "returns an empty list for no item found" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      liftIO $ createItem userId "black" "electronics" "laptop"
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/other" [("token", getToken loginResponse)] ""

      response `shouldRespondWith` [json|[]|] {matchStatus = 200}

    it "returns items created by the current user" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      otherUserId <- liftIO $ createUser "other@other.com" "other"
      (resItemId, resUserId, resDrawerId, resColorId, resItemTypeId, resItemName) <- liftIO $ createItem userId "black" "electronics" "laptop"
      liftIO $ createItem otherUserId "red" "clothes" "laptop"
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/laptop" [("token", getToken loginResponse)] ""

      response
        `shouldRespondWith` [json|[{
        id: #{resItemId},
        user_id: #{resUserId},
        drawer_id: #{resDrawerId},
        color_id: #{resColorId},
        item_type_id: #{resItemTypeId},
        name: #{resItemName}
      }]|]
          { matchStatus = 200
          }

    it "returns list of items " $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      (resItemId1, resUserId1, resDrawerId1, resColorId1, resItemTypeId1, resItemName1) <- liftIO $ createItem userId "black" "electronics" "other name"
      (resItemId2, resUserId2, resDrawerId2, resColorId2, resItemTypeId2, resItemName2) <- liftIO $ createItem userId "green" "cables" "name"
      liftIO $ createItem userId "red" "clothes" "something else"
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/nam" [("token", getToken loginResponse)] ""

      response
        `shouldRespondWith` [json|[{
        id: #{resItemId2},
        user_id: #{resUserId2},
        drawer_id: #{resDrawerId2},
        color_id: #{resColorId2},
        item_type_id: #{resItemTypeId2},
        name: #{resItemName2}
      }, {
        id: #{resItemId1},
        user_id: #{resUserId1},
        drawer_id: #{resDrawerId1},
        color_id: #{resColorId1},
        item_type_id: #{resItemTypeId1},
        name: #{resItemName1}
      }]|]
          { matchStatus = 200
          }
