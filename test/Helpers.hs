module Helpers
  ( loginUser
  , getToken )

where

import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Test as WT
import Data.ByteString.Internal
import Test.Hspec.Wai (WaiSession, postHtmlForm)

import GHC.Generics
import Data.Aeson (FromJSON, decode)

newtype LoginResponse = LoginResponse { token :: String } deriving (Generic, Show)
instance FromJSON LoginResponse

loginUser :: WaiSession () BSL.ByteString
loginUser = WT.simpleBody <$> postHtmlForm "/login" [("email", "user@user.com"), ("password", "password")]

getToken :: BSL.ByteString -> ByteString
getToken loginResponse = packChars $ maybe "" token (decode loginResponse)
