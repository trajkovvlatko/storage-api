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
import Models.Drawer (Drawer (dId, dLevel, dNote, dStorageUnitId), DrawerId, DrawerLevel, DrawerNote)
import Models.Item (ItemName)
import Models.ItemType (ItemTypeId, ItemTypeLabel)
import Models.Room (Room (rId, rName), RoomId, RoomName)
import Models.StorageUnit (StorageUnit (sId, sName, sRoomId), StorageUnitId, StorageUnitName)

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
  results :: [(Integer, RoomName)] <- withConn (\conn -> query conn queryString (userId, name))
  return $ head results

createStorageUnit :: UserId -> String -> Maybe Room -> IO (StorageUnitId, RoomId, String)
createStorageUnit userId name maybeRoom = do
  (roomId, _) <- liftIO $ case maybeRoom of
    Nothing -> createRoom userId "room1"
    Just r -> return (rId r, rName r)
  let queryString =
        [sql| INSERT INTO storage_units (user_id, room_id, name)
              VALUES (?, ?, ?)
              RETURNING id, room_id, name|]
  results :: [(StorageUnitId, RoomId, StorageUnitName)] <- withConn (\conn -> query conn queryString (userId, roomId, name))
  return $ head results

createDrawer :: UserId -> DrawerLevel -> DrawerNote -> Maybe StorageUnit -> IO (DrawerId, StorageUnitId, DrawerLevel, DrawerNote)
createDrawer userId level note maybeStorageUnit = do
  (storageUnitId, _, _) <- liftIO $ case maybeStorageUnit of
    Nothing -> createStorageUnit userId "storageUnit1" Nothing
    Just su -> return (sId su, sRoomId su, sName su)
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

createItem :: UserId -> ColorLabel -> ItemTypeLabel -> ItemName -> Maybe Drawer -> IO (Integer, UserId, DrawerId, ColorId, ItemTypeId, ItemName)
createItem userId colorLabel itemTypeLabel itemName maybeDrawer = do
  (colorId, _) <- createColor colorLabel
  (itemTypeId, _) <- createItemType itemTypeLabel
  (drawerId, _, _, _) <- liftIO $ case maybeDrawer of
    Nothing -> createDrawer userId 1 "drawer1" Nothing
    Just d -> return (dId d, dStorageUnitId d, dLevel d, dNote d)

  let queryString =
        [sql| INSERT INTO items (user_id, drawer_id, color_id, item_type_id, name)
              VALUES (?, ?, ?, ?, ?)
              RETURNING id, user_id, drawer_id, color_id, item_type_id, name|]

  results :: [(Integer, UserId, DrawerId, ColorId, ItemTypeId, ItemName)] <- withConn (\conn -> query conn queryString (userId, drawerId, colorId, itemTypeId, itemName))
  return $ head results
