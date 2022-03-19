module Controllers.Auth
  ( login,
    register,
  )
where

import ClassyPrelude (MonadIO (liftIO), pack, unpack)
import Data.Password.Bcrypt (Bcrypt, PasswordCheck (PasswordCheckSuccess), PasswordHash (PasswordHash), checkPassword, hashPassword, mkPassword)
import Lib.Auth (encodeUserIdToToken)
import Lib.Error (ErrorResponse (ErrorResponse), eMessage)
import Models.User (Email, Password, User (uId, uPassword), createUser, findUserByEmail)
import Network.HTTP.Types (status401, status422)
import Web.Scotty (ActionM, json, liftAndCatchIO, param, status)

register :: ActionM ()
register = do
  paramEmail :: Email <- param "email"
  paramPassword :: Password <- param "password"
  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing -> status status422 >> json ErrorResponse {eMessage = "Cannot create a user."}
    Just user -> encodeUserIdToToken (uId user)

-- login

login :: ActionM ()
login = do
  paramEmail :: Email <- param "email"
  paramPassword :: Password <- param "password"
  maybeUser <- liftAndCatchIO (findUserByEmail paramEmail)
  loginMaybeUser paramPassword maybeUser

-- helper functions

verify :: Password -> Password -> IO PasswordCheck
verify paramPassword userPassword = do
  let pass = mkPassword $ pack paramPassword
  return $ checkPassword pass (PasswordHash $ pack userPassword)

loginMaybeUser :: Password -> Maybe User -> ActionM ()
loginMaybeUser _ Nothing = json $ ErrorResponse {eMessage = "Cannot find user."}
loginMaybeUser paramPassword (Just user) = do
  valid <- liftIO $ verify paramPassword (uPassword user)
  loginResponse user valid

loginResponse :: User -> PasswordCheck -> ActionM ()
loginResponse user PasswordCheckSuccess = encodeUserIdToToken (uId user)
loginResponse _ _ = status status401 >> json ErrorResponse {eMessage = "Invalid password."}
