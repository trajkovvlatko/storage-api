module Models.Room 
  ( getAllRooms
  , getRoom
  , createRoom
  , updateRoom
  , deleteRoom
  , Room
    ( Room
    , rId
    , rName)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple (query_, query, Only (Only), SqlError)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs)
import Control.Exception (try)
import Lib.Auth (UserId)

data Room = Room
  { rId     :: Integer
  , rName   :: String } deriving Generic

instance FromRow Room where
  fromRow = Room <$> field <*> field

instance ToJSON Room where
  toEncoding (Room rId rName) =
    pairs $    "id"   .= rId
            <> "name" .= rName

-- queries

getAllRooms :: UserId -> IO [Room]
getAllRooms userId = withConn $ \conn -> query conn "SELECT id, name FROM rooms WHERE user_id = ?;" (Only userId)

getRoom :: UserId -> Integer -> IO (Maybe Room)
getRoom userId paramId = do
  withConn $ \conn -> query conn queryString [paramId, userId] >>= resultsToMaybeRoom
  where queryString = "SELECT id, name FROM rooms WHERE id = ? AND user_id = ? LIMIT 1"

createRoom :: UserId -> String -> IO (Maybe Room)
createRoom userId paramName = do
  withConn $ \conn -> query conn queryString [paramName, show userId] >>= resultsToMaybeRoom
  where queryString = "INSERT INTO rooms (name, user_id) VALUES (?, ?) RETURNING id, name"

updateRoom :: UserId -> Integer -> String -> IO (Maybe Room)
updateRoom userId paramId paramName = do
  withConn $ \conn -> query conn queryString (paramName, paramId, userId) >>= resultsToMaybeRoom
  where queryString = "UPDATE rooms SET name = ? WHERE id = ? AND user_id = ? RETURNING id, name"

deleteRoom :: UserId -> Integer -> IO (Maybe Room)
deleteRoom userId paramId = do
  withConn $ \conn -> query conn queryString [paramId, userId] >>= resultsToMaybeRoom
  where queryString = "DELETE FROM rooms WHERE id = ? AND user_id = ? RETURNING id, name"

-- helper functions

resultsToMaybeRoom :: [(Integer, String)] -> IO (Maybe Room)
resultsToMaybeRoom = \case
  [(resId, resName)] -> return $ Just $ Room { rId = resId, rName = resName }
  _                  -> return Nothing