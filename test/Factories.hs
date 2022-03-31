module Factories
  ( createUser,
    createRoom,
    createStorageUnit,
    createDrawer,
    createItemType,
    createColor,
    createItem,
  )
where

import ClassyPrelude (MonadIO (liftIO), fromString, unpack)
import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Database (withConn)
import Database.PostgreSQL.Simple (Only (Only), query)
import Database.PostgreSQL.Simple.SqlQQ
import Lib.Auth (UserId)
import Models.Color (ColorId, ColorLabel)
import Models.Drawer (DrawerId, DrawerLevel, DrawerNote)
import Models.Item (ItemName)
import Models.ItemType (ItemTypeId, ItemTypeLabel)
import Models.Room (RoomId)
import Models.StorageUnit (StorageUnitId)

createUser :: String -> String -> IO UserId
createUser email password = do
  passwordHash <- hashPassword (mkPassword (fromString password))
  let passwordString = unpack $ unPasswordHash passwordHash
      queryString = [sql| INSERT INTO users (email, password) VALUES (?, ?) RETURNING id |]
  [Only userId] <- withConn (\conn -> query conn queryString (email, passwordString))
  return userId

createRoom :: UserId -> String -> IO (Integer, String)
createRoom userId name = do
  let queryString = [sql| INSERT INTO rooms (user_id, name) VALUES (?, ?) RETURNING id, name |]
  results :: [(Integer, String)] <- withConn (\conn -> query conn queryString (userId, name))
  return $ head results

createStorageUnit :: UserId -> String -> IO (StorageUnitId, RoomId, String)
createStorageUnit userId name = do
  (roomId, _) <- liftIO $ createRoom userId "room1"
  let queryString =
        [sql| INSERT INTO storage_units (user_id, room_id, name)
              VALUES (?, ?, ?)
              RETURNING id, room_id, name|]
  results :: [(StorageUnitId, RoomId, String)] <- withConn (\conn -> query conn queryString (userId, roomId, name))
  return $ head results

createDrawer :: UserId -> DrawerLevel -> DrawerNote -> IO (DrawerId, StorageUnitId, DrawerLevel, DrawerNote)
createDrawer userId level note = do
  (storageUnitId, _, _) <- liftIO $ createStorageUnit userId "storageUnit1"
  let queryString =
        [sql| INSERT INTO drawers (user_id, storage_unit_id, level, note)
              VALUES (?, ?, ?, ?)
              RETURNING id, storage_unit_id, level, note|]
  results :: [(DrawerId, StorageUnitId, DrawerLevel, DrawerNote)] <- withConn (\conn -> query conn queryString (userId, storageUnitId, level, note))
  return $ head results

createItemType :: String -> IO (Integer, String)
createItemType label = do
  let queryString =
        [sql| INSERT INTO item_types (label)
              VALUES (?)
              RETURNING id, label|]
  results :: [(Integer, String)] <- withConn (\conn -> query conn queryString [label])
  return $ head results

createColor :: String -> IO (Integer, String)
createColor label = do
  let queryString =
        [sql| INSERT INTO colors (label)
              VALUES (?)
              RETURNING id, label|]
  results :: [(Integer, String)] <- withConn (\conn -> query conn queryString [label])
  return $ head results

createItem :: UserId -> ColorLabel -> ItemTypeLabel -> ItemName -> IO (Integer, UserId, DrawerId, ColorId, ItemTypeId, ItemName)
createItem userId colorLabel itemTypeLabel itemName = do
  (colorId, _) <- createColor colorLabel
  (itemTypeId, _) <- createItemType itemTypeLabel
  (drawerId, _, _, _) <- liftIO $ createDrawer userId 1 "drawer1"

  let queryString =
        [sql| INSERT INTO items (user_id, drawer_id, color_id, item_type_id, name)
              VALUES (?, ?, ?, ?, ?)
              RETURNING id, user_id, drawer_id, color_id, item_type_id, name|]

  results :: [(Integer, UserId, DrawerId, ColorId, ItemTypeId, ItemName)] <- withConn (\conn -> query conn queryString (userId, drawerId, colorId, itemTypeId, itemName))
  return $ head results
