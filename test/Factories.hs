module Factories
  ( createUser
  , createRoom )

where

import Lib.Auth (UserId)
import Database (withConn)
import Database.PostgreSQL.Simple (Only (Only), query_)
import ClassyPrelude (fromString)
import Data.Hash.MD5 ( md5s, Str(Str) )

createUser :: String -> String -> IO UserId
createUser email password = do
  [Only userId] <- withConn (\conn -> query_  conn (fromString queryString))
  return userId
  where
    md5Password = md5s $ Str password
    queryString = "INSERT INTO users (email, password) VALUES ('" ++ email ++ "', '" ++ md5Password ++ "') RETURNING id"

createRoom :: UserId -> String -> IO (Integer, String)
createRoom userId name = do
  results :: [(Integer, String)] <- withConn (\conn -> query_  conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO rooms (user_id, name) VALUES ('" ++ show userId ++ "', '" ++ name ++ "') RETURNING id, name"
