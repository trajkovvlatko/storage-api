module Controllers.ColorsSpec (spec) where

import ClassyPrelude (IsString (fromString), MonadIO (liftIO))
import Controllers.Colors
import Factories (createColor, createUser)
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
        let response = get "/colors"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no colors found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser

        let response = request "GET" "/colors" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns a list of colors" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (colorId, colorLabel) <- liftIO $ createColor "color1"

        let response = request "GET" "/colors" [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[{id: #{colorId}, label: #{colorLabel}}]|] {matchStatus = 200}

  describe "preview" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let url = fromString $ "/colors/" ++ show colorId

        let response = get url

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "returns an empty list for no colors found" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString "/colors/-1"

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|[]|] {matchStatus = 200}

      it "returns an color" $ do
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        liftIO $ createColor "color0"
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let url = fromString $ "/colors/" ++ show colorId

        let response = request "GET" url [("token", getToken loginResponse)] ""

        response `shouldRespondWith` [json|{id: #{colorId}, label: #{colorLabel}}|] {matchStatus = 200}

  describe "create" $ do
    it "responds with 500 for missing parameter" $ do
      post "/colors" "" `shouldRespondWith` 500

    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        let postBody = "label=2323"
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPost "/colors" headers postBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 200" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "label=color-label"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/colors" headers postBody

        response `shouldRespondWith` 200

      it "creates a color" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        let postBody = "label=color-label"
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPost "/colors" headers postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "label\":\"color-label\""

  describe "update" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let patchBody = "label=updated-label"
        let url = fromString $ "/colors/" ++ show colorId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodPatch url headers patchBody

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "responds with 200 for missing parameter" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let headers = [contentType, ("token", getToken loginResponse)]
        let url = fromString $ "/colors/" ++ show colorId

        let response = request methodPatch url headers ""

        response `shouldRespondWith` 200

      it "updates a color" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let patchBody = "label=updated-label"
        let url = fromString $ "/colors/" ++ show colorId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodPatch url headers patchBody
        body <- fmap WT.simpleBody response

        liftIO $ body `shouldContainString` "label\":\"updated-label\""
        response `shouldRespondWith` 200

  describe "delete" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        liftIO $ createUser "user@user.com" "password"
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let url = fromString $ "/colors/" ++ show colorId
        let headers = [("Content-Type", "application/x-www-form-urlencoded")]

        let response = request methodDelete url headers ""

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

    context "with authenticated user" $ do
      it "deletes a color" $ do
        liftIO $ createUser "user@user.com" "password"
        loginResponse <- loginUser
        (colorId, colorLabel) <- liftIO $ createColor "color1"
        let url = fromString $ "/colors/" ++ show colorId
        let headers = [contentType, ("token", getToken loginResponse)]

        let response = request methodDelete url headers ""

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "label\":\"color1\""
        response `shouldRespondWith` 200
