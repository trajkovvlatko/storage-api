module Models.Drawer 
  ( getAllDrawers
  , getDrawer
  , createDrawer
  , updateDrawer
  , deleteDrawer
  , Drawer
    ( Drawer
    , dId
    , dLevel
    , dNote)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple (query_, query, Only (Only), SqlError)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs)
import Control.Exception (try)
import Lib.Auth (UserId)

data Drawer = Drawer
  { dId        :: Integer
  , dStorageId :: Integer
  , dLevel     :: Integer
  , dNote      :: String } deriving Generic

instance FromRow Drawer where
  fromRow = Drawer <$> field <*> field <*> field <*> field

instance ToJSON Drawer where
  toEncoding (Drawer dId dStorageId dLevel dNote) =
    pairs $    "id"         .= dId
            <> "storage_id" .= dStorageId
            <> "level"      .= dLevel
            <> "note"       .= dNote

-- queries

getAllDrawers :: UserId -> Integer -> IO [Drawer]
getAllDrawers userId paramStorageId = do
  withConn $ \conn -> query conn queryString (userId, paramStorageId)
  where queryString = "SELECT id, storage_id, level, note FROM drawers WHERE user_id = ? AND storage_id = ?;"

getDrawer :: UserId -> Integer -> IO (Maybe Drawer)
getDrawer userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeDrawer
  where queryString = "SELECT id, storage_id, level, note FROM drawers WHERE id = ? AND user_id = ? LIMIT 1"

createDrawer :: UserId -> Integer -> Integer -> String -> IO (Maybe Drawer)
createDrawer userId paramStorageId paramLevel paramNote = do
  withConn $ \conn -> query conn queryString (userId, paramStorageId, paramLevel, paramNote) >>= resultsToMaybeDrawer
  where queryString = "INSERT INTO drawers (user_id, storage_id, level, note) VALUES (?, ?, ?, ?) RETURNING id, storage_id, level, note"

updateDrawer :: UserId -> Integer -> Integer -> Integer -> String -> IO (Maybe Drawer)
updateDrawer userId paramId paramStorageId paramLevel paramNote = do
  withConn $ \conn -> query conn queryString (paramStorageId, paramLevel, paramNote, paramId, userId) >>= resultsToMaybeDrawer
  where queryString = "UPDATE drawers SET storage_id = ?, level = ?, note = ? WHERE id = ? AND user_id = ? RETURNING id, storage_id, level, note"

deleteDrawer :: UserId -> Integer -> IO (Maybe Drawer)
deleteDrawer userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeDrawer
  where queryString = "DELETE FROM drawers WHERE id = ? AND user_id = ? RETURNING id, storage_id, level note"

-- helper functions

resultsToMaybeDrawer :: [(Integer, Integer, Integer, String)] -> IO (Maybe Drawer)
resultsToMaybeDrawer = \case
  [(resId, resStorageId, resLevel, resNote)] -> return $ Just $ Drawer { dId = resId
                                                                       , dStorageId = resStorageId
                                                                       , dLevel = resLevel
                                                                       , dNote = resNote }
  _ -> return Nothing