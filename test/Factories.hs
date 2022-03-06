module Factories
  ( createUser
  , createRoom )

where

import Lib.Auth (UserId)
import Database (withConn)
import Database.PostgreSQL.Simple (Only (Only), query_)
import ClassyPrelude (fromString, unpack)
import Data.Password.Bcrypt (hashPassword, mkPassword, PasswordHash (unPasswordHash))

createUser :: String -> String -> IO UserId
createUser email password = do
  passwordHash <- hashPassword (mkPassword (fromString password))
  let passwordString = unpack $ unPasswordHash passwordHash
      queryString = "INSERT INTO users (email, password) VALUES ('" ++ email ++ "', '" ++ passwordString ++ "') RETURNING id"
  [Only userId] <- withConn (\conn -> query_  conn (fromString queryString))
  return userId

createRoom :: UserId -> String -> IO (Integer, String)
createRoom userId name = do
  results :: [(Integer, String)] <- withConn (\conn -> query_  conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO rooms (user_id, name) VALUES ('" ++ show userId ++ "', '" ++ name ++ "') RETURNING id, name"
