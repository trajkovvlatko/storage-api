module Controllers.RoomsSpec
  ( spec )

where

import Server ( app )
import Database (withConn)
import Data.Hash.MD5 ( md5s, Str(Str) )
import ClassyPrelude (fromString)

import Controllers.Rooms

import Test.Hspec
import Test.Hspec.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec.Wai.JSON
import Database.PostgreSQL.Simple (query, execute_)

import GHC.Generics
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL

newtype LoginResponse = LoginResponse { token :: String } deriving (Generic, Show)
instance FromJSON LoginResponse

createUser :: IO ()
createUser = do
  withConn (`execute_` fromString queryString)
  return ()
  where
    email = "user@user.com"
    password = "5f4dcc3b5aa765d61d8327deb882cf99" -- md5 "password"
    queryString = "INSERT INTO users (email, password) VALUES ('" ++ email ++ "', '" ++ password ++ "')"

loginUser :: WaiSession () BSL.ByteString
loginUser = WT.simpleBody <$> postHtmlForm "/login" [("email", "user@user.com"), ("password", "password")]

getToken :: BSL.ByteString -> ByteString
getToken loginResponse = packChars $  maybe "" token (decode loginResponse)

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
        liftIO createUser
        loginResponse <- loginUser
        request "GET" "/rooms" [("token", getToken loginResponse)] "" `shouldRespondWith` [json|[]|]
