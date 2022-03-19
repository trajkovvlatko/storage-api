module Helpers
  ( loginUser
  , getToken
  , shouldContainString )

where

import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Test as WT
import Data.ByteString.Internal
import Test.Hspec.Wai (WaiSession, postHtmlForm)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Test.HUnit (assertBool)

import GHC.Generics
import Data.Aeson (FromJSON, decode)

newtype LoginResponse = LoginResponse { token :: String } deriving (Generic, Show)
instance FromJSON LoginResponse

loginUser :: WaiSession () BSL.ByteString
loginUser = WT.simpleBody <$> postHtmlForm "/login" [("email", "user@user.com"), ("password", "password")]

getToken :: BSL.ByteString -> ByteString
getToken loginResponse = packChars $ maybe "" token (decode loginResponse)

shouldContainString :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContainString subject matcher = assertBool message (subject `contains` matcher)
  where
    s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
    message  =
      "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but not"
