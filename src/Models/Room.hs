module Models.Room 
  ( getAllRooms
  , getRoom
  , createRoom
  , updateRoom
  , Room
    ( Room
    , rId
    , rName)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple ( query_, query, Only (Only) )
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson ( ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs )

data Room = Room
  { rId     :: Int
  , rName   :: String } deriving Generic

instance FromRow Room where
  fromRow = Room <$> field <*> field

instance ToJSON Room where
  toEncoding (Room rId rName) =
    pairs $    "id"   .= rId
            <> "name" .= rName

-- queries

getAllRooms :: IO [Room]
getAllRooms = withConn $ \conn -> query_ conn "SELECT id, name FROM rooms;"

getRoom :: Int -> IO (Maybe Room)
getRoom paramId = do
  results <- withConn $ \conn -> query conn "SELECT * FROM rooms WHERE id = ? LIMIT 1" (Only paramId)
  resultsToMaybeRoom results

createRoom :: String -> IO (Maybe Room)
createRoom paramName = do
  results <- withConn $ \conn -> query conn "INSERT INTO rooms (name) VALUES (?) RETURNING id, name" [paramName]
  resultsToMaybeRoom results

updateRoom :: Int -> String -> IO (Maybe Room)
updateRoom paramId paramName = do
  results <- withConn $ \conn -> query conn "UPDATE rooms SET name = ? WHERE id = ? RETURNING id, name" (paramName, paramId)
  resultsToMaybeRoom results

-- helper functions

resultsToMaybeRoom :: [(Int, String)] -> IO (Maybe Room)
resultsToMaybeRoom = \case
  [(resId, resName)] ->
    return $ Just $ Room resId resName
  _ ->
    return Nothing