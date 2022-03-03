module Models.Storage 
  ( getAllStorages
  , getStorage
  , createStorage
  , updateStorage
  , deleteStorage
  , Storage
    ( Storage
    , sId
    , sName)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple (query_, query, Only (Only), SqlError)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs)
import Control.Exception (try)
import Lib.Auth (UserId)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import ClassyPrelude (IsString(fromString))
import Web.Scotty (liftAndCatchIO)

data Storage = Storage
  { sId     :: Integer
  , sRoomId :: Integer
  , sName   :: String } deriving Generic

instance FromRow Storage where
  fromRow = Storage <$> field <*> field <*> field

instance ToJSON Storage where
  toEncoding (Storage sId sRoomId sName) =
    pairs $    "id"      .= sId
            <> "room_id" .= sRoomId
            <> "name"    .= sName

-- queries

getAllStorages :: UserId -> Integer -> IO [Storage]
getAllStorages userId paramRoomId = do
  withConn $ \conn -> query conn queryString (userId, paramRoomId)
  where queryString = "SELECT id, room_id, name FROM storages WHERE user_id = ? AND room_id = ?;"

getStorage :: UserId -> Integer -> IO (Maybe Storage)
getStorage userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeStorage
  where queryString = "SELECT id, room_id, name FROM storages WHERE id = ? AND user_id = ? LIMIT 1"

createStorage :: UserId -> Integer -> String -> IO (Maybe Storage)
createStorage userId paramRoomId paramName = do
  withConn $ \conn -> query conn queryString (userId, paramRoomId, paramName) >>= resultsToMaybeStorage
  where queryString = "INSERT INTO storages (user_id, room_id, name) VALUES (?, ?, ?) RETURNING id, room_id, name"

updateStorage :: UserId -> Integer -> Maybe Integer -> Maybe String -> IO (Maybe Storage)
updateStorage userId paramId paramRoomId paramName = do
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
        updateQueryString = "UPDATE storages SET " <> updatesString <> " WHERE id = ? AND user_id = ? RETURNING id, room_id, name"
        selectQueryString = "SELECT id, room_id, name FROM storages WHERE id = ? AND user_id = ? LIMIT 1"

    print updateQueryString
    resultsToMaybeStorage =<< if L.null updateList
      then query conn selectQueryString (paramId, userId)
      else query conn updateQueryString paramList

deleteStorage :: UserId -> Integer -> IO (Maybe Storage)
deleteStorage userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeStorage
  where queryString = "DELETE FROM storages WHERE id = ? AND user_id = ? RETURNING id, room_id, name"

-- helper functions

resultsToMaybeStorage :: [(Integer, Integer, String)] -> IO (Maybe Storage)
resultsToMaybeStorage = \case
  [(resId, resRoomId, resName)] -> return $ Just $ Storage { sId = resId, sRoomId = resRoomId, sName = resName }
  _                             -> return Nothing
