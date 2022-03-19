module Controllers.AuthSpec
  ( spec )

where

import Server ( app )
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Factories (createUser)
import Helpers (shouldContainString)
import Data.ByteString (unpack, pack)
import ClassyPrelude (IsString(fromString), MonadIO (liftIO))
import Network.HTTP.Types
import Test.Hspec
import qualified Network.Wai.Test as WT

contentType = ("Content-Type", "application/x-www-form-urlencoded")

spec :: Spec
spec = with app $ do
  describe "register" $ do
    it "returns an error 500 for missing parameters" $ do
      let response = request methodPost "/register" [contentType] ""

      response `shouldRespondWith` 500

    it "returns 200 for valid parameters" $ do
      let postBody = "email=email@email.com&password=password1"
      let response = request methodPost "/register" [contentType] postBody

      response `shouldRespondWith` 200

    it "returns a token for valid params" $ do
      let postBody = "email=email@email.com&password=password1"
      let response = request methodPost "/register" [contentType] postBody

      body <- fmap WT.simpleBody response
      liftIO $ body `shouldContainString` "token\":\""

    it "returns an error 422 for duplicate email" $ do
      liftIO $ createUser "email@email.com" "password"
      let postBody = "email=email@email.com&password=password1"
      let response = request methodPost "/register" [contentType] postBody

      response `shouldRespondWith` 422

  describe "login" $ do
    let email = "email@email.com"
    let password = "password"

    context "for invalid params" $ do
      let postBody = fromString $ "email=" ++ email ++ "&password=invalid-password"

      it "returns an error for invalid params" $ do
        liftIO $ createUser email password
        let response = request methodPost "/login" [contentType] postBody

        response `shouldRespondWith` [json|{message: "Invalid password."}|] {matchStatus = 401}

    context "for valid params" $ do
      let postBody = fromString $ "email=" ++ email ++ "&password=" ++ password

      it "returns 200" $ do
        liftIO $ createUser email password
        let response = request methodPost "/login" [contentType] postBody

        response `shouldRespondWith` 200

      it "returns a token" $ do
        liftIO $ createUser email password
        let response = request methodPost "/login" [contentType] postBody

        body <- fmap WT.simpleBody response
        liftIO $ body `shouldContainString` "token\":\""
