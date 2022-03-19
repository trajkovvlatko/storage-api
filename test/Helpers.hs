module Helpers
  ( loginUser,
    getToken,
    shouldContainString,
  )
where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import GHC.Generics
import qualified Network.Wai.Test as WT
import Test.HUnit (assertBool)
import Test.Hspec
import Test.Hspec.Wai (WaiSession, postHtmlForm)

newtype LoginResponse = LoginResponse {token :: String} deriving (Generic, Show)

instance FromJSON LoginResponse

loginUser :: WaiSession () BSL.ByteString
loginUser = WT.simpleBody <$> postHtmlForm "/login" [("email", "user@user.com"), ("password", "password")]

getToken :: BSL.ByteString -> ByteString
getToken loginResponse = packChars $ maybe "" token (decode loginResponse)

shouldContainString :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContainString subject matcher = assertBool message (subject `contains` matcher)
  where
    s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
    message =
      "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but it didn't"
