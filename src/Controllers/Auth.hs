module Controllers.Auth
  ( login
  , register
  ) where

import Web.Scotty (ActionM, liftAndCatchIO, param, json, status)
import Lib.Auth (encodeUserIdToToken)
import Models.User (createUser, findUserByEmail, User (uId, uPassword))
import Lib.Error (ErrorResponse (ErrorResponse), eMessage)
import Data.Password.Bcrypt (PasswordCheck(PasswordCheckSuccess), mkPassword, hashPassword, checkPassword, PasswordHash (PasswordHash), Bcrypt)
import ClassyPrelude (MonadIO(liftIO), pack, unpack)
import Network.HTTP.Types (status422, status401)

register :: ActionM ()
register = do
  paramEmail :: String    <- param "email"
  paramPassword :: String <- param "password"
  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing -> status status422 >> json ErrorResponse { eMessage = "Cannot create a user." }
    Just user -> encodeUserIdToToken (uId user)

-- login

login :: ActionM ()
login = do
  paramEmail :: String    <- param "email"
  paramPassword :: String <- param "password"
  maybeUser <- liftAndCatchIO (findUserByEmail paramEmail)
  loginMaybeUser paramPassword maybeUser
  
-- helper functions

verify :: String -> String -> IO PasswordCheck
verify paramPassword userPassword = do
  let pass = mkPassword $ pack paramPassword
  return $ checkPassword pass (PasswordHash $ pack userPassword)

loginMaybeUser :: String -> Maybe User -> ActionM ()
loginMaybeUser _ Nothing = json $ ErrorResponse { eMessage = "Cannot find user." }
loginMaybeUser paramPassword (Just user) = do
  valid <- liftIO $ verify paramPassword (uPassword user)
  loginResponse user valid

loginResponse :: User -> PasswordCheck -> ActionM ()
loginResponse user PasswordCheckSuccess = encodeUserIdToToken (uId user)
loginResponse _    _                    = status status401 >> json ErrorResponse { eMessage = "Invalid password." }
