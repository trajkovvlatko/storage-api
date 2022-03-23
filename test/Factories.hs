module Factories
  ( createUser,
    createRoom,
    createStorageUnit,
    createDrawer,
    createItemType,
    createColor,
  )
where

import ClassyPrelude (fromString, unpack)
import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Database (withConn)
import Database.PostgreSQL.Simple (Only (Only), query_)
import Lib.Auth (UserId)

createUser :: String -> String -> IO UserId
createUser email password = do
  passwordHash <- hashPassword (mkPassword (fromString password))
  let passwordString = unpack $ unPasswordHash passwordHash
      queryString = "INSERT INTO users (email, password) VALUES ('" ++ email ++ "', '" ++ passwordString ++ "') RETURNING id"
  [Only userId] <- withConn (\conn -> query_ conn (fromString queryString))
  return userId

createRoom :: UserId -> String -> IO (Integer, String)
createRoom userId name = do
  results :: [(Integer, String)] <- withConn (\conn -> query_ conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO rooms (user_id, name) VALUES ('" ++ show userId ++ "', '" ++ name ++ "') RETURNING id, name"

createStorageUnit :: UserId -> Integer -> String -> IO (Integer, String)
createStorageUnit userId roomId name = do
  results :: [(Integer, String)] <- withConn (\conn -> query_ conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO storage_units (user_id, room_id, name) VALUES ('" ++ show userId ++ "', '" ++ show roomId ++ "', '" ++ name ++ "') RETURNING id, name"

createDrawer :: UserId -> Integer -> Integer -> String -> IO (Integer, Integer, String)
createDrawer userId storageUnitId level note = do
  results :: [(Integer, Integer, String)] <- withConn (\conn -> query_ conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO drawers (user_id, storage_unit_id, level, note) VALUES ('" ++ show userId ++ "', '" ++ show storageUnitId ++ "', '" ++ show level ++ "', '" ++ note ++ "') RETURNING id, level, note"

createItemType :: String -> IO (Integer, String)
createItemType label = do
  results :: [(Integer, String)] <- withConn (\conn -> query_ conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO item_types (label) VALUES ('" ++ label ++ "') RETURNING id, label"

createColor :: String -> IO (Integer, String)
createColor label = do
  results :: [(Integer, String)] <- withConn (\conn -> query_ conn (fromString queryString))
  return $ head results
  where
    queryString = "INSERT INTO colors (label) VALUES ('" ++ label ++ "') RETURNING id, label"
