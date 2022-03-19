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
import Database.PostgreSQL.Simple (query, Only (Only))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toEncoding), KeyValue((.=)), pairs)
import Lib.Auth (UserId)

data Room = Room
  { rId     :: Integer
  , rUserId :: UserId
  , rName   :: String } deriving Generic

instance FromRow Room where
  fromRow = Room <$> field <*> field <*> field

instance ToJSON Room where
  toEncoding (Room id' userId name) =
    pairs $    "id"      .= id'
            <> "user_id" .= userId
            <> "name"    .= name

-- queries

getAllRooms :: UserId -> IO [Room]
getAllRooms userId = do
  withConn $ \conn -> query conn queryString (Only userId)
  where queryString = "SELECT id, user_id, name FROM rooms WHERE user_id = ?;"

getRoom :: UserId -> Integer -> IO (Maybe Room)
getRoom userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeRoom
  where queryString = "SELECT id, user_id, name FROM rooms WHERE id = ? AND user_id = ? LIMIT 1"

createRoom :: UserId -> String -> IO (Maybe Room)
createRoom userId paramName = do
  withConn $ \conn -> query conn queryString (paramName, userId) >>= resultsToMaybeRoom
  where queryString = "INSERT INTO rooms (name, user_id) VALUES (?, ?) RETURNING id, user_id, name"

updateRoom :: UserId -> Integer -> String -> IO (Maybe Room)
updateRoom userId paramId paramName = do
  withConn $ \conn -> query conn queryString (paramName, paramId, userId) >>= resultsToMaybeRoom
  where queryString = "UPDATE rooms SET name = ? WHERE id = ? AND user_id = ? RETURNING id, user_id, name"

deleteRoom :: UserId -> Integer -> IO (Maybe Room)
deleteRoom userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeRoom
  where queryString = "DELETE FROM rooms WHERE id = ? AND user_id = ? RETURNING id, user_id, name"

-- helper functions

resultsToMaybeRoom :: [(Integer, UserId, String)] -> IO (Maybe Room)
resultsToMaybeRoom = \case
  [(resId, resUserId, resName)] -> return $ Just $ Room { rId = resId, rUserId = resUserId, rName = resName }
  _ -> return Nothing
