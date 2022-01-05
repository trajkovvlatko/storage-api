{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Controllers.Auth
  ( login
  , register
  ) where

import GHC.Generics (Generic)
import Web.Scotty (ActionM, text, liftAndCatchIO, param, json, status)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (Text)
import Models.User (createUser, findUserByEmail, User (uId, uPassword))
import ClassyPrelude (unpack, pack, Utf8(encodeUtf8), IsString (fromString), Text)
import Data.Aeson (ToJSON(toJSON, toEncoding), KeyValue((.=)), pairs)
import Data.Hash.MD5 ( md5s, Str(Str) )

newtype ErrorResponse = ErrorResponse { eMessage :: String } deriving Generic

instance ToJSON ErrorResponse where
  toEncoding (ErrorResponse eMessage) =
    pairs $ "message" .= eMessage

newtype TokenResponse = TokenResponse { uToken :: ClassyPrelude.Text } deriving Generic

instance ToJSON TokenResponse where
  toEncoding (TokenResponse uToken) =
    pairs $ "token" .= uToken

register :: ActionM ()
register = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"

  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing   -> json $ ErrorResponse { eMessage = "Cannot create a user." }
    Just user -> do
      encoded <- (liftAndCatchIO . userIdToToken) $ uId user
      case encoded of
        Left _              -> json $ ErrorResponse { eMessage = "Cannot encode user token." }
        Right encodedUserId -> json $ TokenResponse { uToken = encodedUserId }

-- login

login :: ActionM ()
login = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"
  liftAndCatchIO (findUserByEmail paramEmail) >>= loginMaybeUser paramPassword
  
-- helper functions

loginMaybeUser :: String -> Maybe User -> ActionM ()
loginMaybeUser _ Nothing = json $ ErrorResponse { eMessage = "Cannot find user." }
loginMaybeUser paramPassword (Just user) = loginResponse user valid
  where valid = uPassword user == md5s (Str paramPassword)

loginResponse :: User -> Bool -> ActionM ()
loginResponse _    False = json $ ErrorResponse { eMessage = "Invalid password." }
loginResponse user True  = do
  encoded <- (liftAndCatchIO . userIdToToken) $ uId user
  case encoded of
    Left _              -> json $ ErrorResponse { eMessage = "Cannot encode user token." }
    Right encodedUserId -> json $ TokenResponse { uToken = encodedUserId }

integerToText :: Integer -> ClassyPrelude.Text
integerToText = pack . show