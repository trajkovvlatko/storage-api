{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Controllers.Auth
  ( login
  , register
  ) where

import Web.Scotty (ActionM, text, liftAndCatchIO, param, json, status)
import Lib.Auth (encodeUserIdToToken)
import Data.Text.Lazy (Text)
import Models.User (createUser, findUserByEmail, User (uId, uPassword))
import ClassyPrelude (unpack, pack, Utf8(encodeUtf8), IsString (fromString), Text)
import Lib.Error (ErrorResponse (ErrorResponse), eMessage)
import Data.Hash.MD5 ( md5s, Str(Str) )

register :: ActionM ()
register = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"
  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing   -> json $ ErrorResponse { eMessage = "Cannot create a user." }
    Just user -> encodeUserIdToToken (uId user)

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
loginResponse user True  = encodeUserIdToToken (uId user)

-- helpers

integerToText :: Integer -> ClassyPrelude.Text
integerToText = pack . show