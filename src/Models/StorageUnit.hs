module Models.StorageUnit
  ( getAllStorageUnits
  , getStorageUnit
  , createStorageUnit
  , updateStorageUnit
  , deleteStorageUnit
  , StorageUnit
    ( StorageUnit
    , sId
    , sName)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple (query)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toEncoding), KeyValue((.=)), pairs)
import Lib.Auth (UserId)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Simple.ToField (ToField(toField))

data StorageUnit = StorageUnit
  { sId     :: Integer
  , sRoomId :: Integer
  , sName   :: String } deriving Generic

instance FromRow StorageUnit where
  fromRow = StorageUnit <$> field <*> field <*> field

instance ToJSON StorageUnit where
  toEncoding (StorageUnit id' roomId name) =
    pairs $    "id"      .= id'
            <> "room_id" .= roomId
            <> "name"    .= name

-- queries

getAllStorageUnits :: UserId -> Integer -> IO [StorageUnit]
getAllStorageUnits userId paramRoomId = do
  withConn $ \conn -> query conn queryString (userId, paramRoomId)
  where queryString = "SELECT id, room_id, name FROM storage_units WHERE user_id = ? AND room_id = ?;"

getStorageUnit :: UserId -> Integer -> IO (Maybe StorageUnit)
getStorageUnit userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeStorage
  where queryString = "SELECT id, room_id, name FROM storage_units WHERE id = ? AND user_id = ? LIMIT 1"

createStorageUnit :: UserId -> Integer -> String -> IO (Maybe StorageUnit)
createStorageUnit userId paramRoomId paramName = do
  withConn $ \conn -> query conn queryString (userId, paramRoomId, userId, paramName) >>= resultsToMaybeStorage
  where queryString = "INSERT INTO storage_units (user_id, room_id, name) VALUES (?, (SELECT id FROM rooms where id = ? AND user_id = ?), ?) RETURNING id, room_id, name"

updateStorageUnit :: UserId -> Integer -> Maybe Integer -> Maybe String -> IO (Maybe StorageUnit)
updateStorageUnit userId paramId paramRoomId paramName = do
  withConn $ \conn -> do
    let updateList = catMaybes
          [ const "room_id = ?" <$> paramRoomId
          , const "name = ?" <$> paramName ]
        paramList = catMaybes
          [ toField <$> paramRoomId
          , toField <$> paramName
          , toField <$> Just paramId
          , toField <$> Just userId ]
        updatesString = if L.null updateList then mempty else mconcat $ L.intersperse ", " updateList
        updateQueryString = "UPDATE storage_units SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, room_id, name"
        selectQueryString = "SELECT id, room_id, name FROM storage_units WHERE id = ? AND user_id = ? LIMIT 1"

    resultsToMaybeStorage =<< if L.null updateList
      then query conn selectQueryString (paramId, userId)
      else query conn updateQueryString paramList

deleteStorageUnit :: UserId -> Integer -> IO (Maybe StorageUnit)
deleteStorageUnit userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeStorage
  where queryString = "DELETE FROM storage_units WHERE id = ? AND user_id = ? RETURNING id, room_id, name"

-- helper functions

resultsToMaybeStorage :: [(Integer, Integer, String)] -> IO (Maybe StorageUnit)
resultsToMaybeStorage = \case
  [(resId, resRoomId, resName)] -> return $ Just $ StorageUnit { sId = resId, sRoomId = resRoomId, sName = resName }
  _                             -> return Nothing
