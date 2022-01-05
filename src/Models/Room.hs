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

getAllRooms :: IO [Room]
getAllRooms = withConn $ \conn -> query_ conn "SELECT id, name FROM rooms;"

getRoom :: Int -> IO (Maybe Room)
getRoom paramId = do
  results <- withConn $ \conn -> try $  query conn "SELECT * FROM rooms WHERE id = ? LIMIT 1" (Only paramId)
  resultsToMaybeRoom results

createRoom :: String -> IO (Maybe Room)
createRoom paramName = do
  results <- withConn $ \conn -> try $ query conn "INSERT INTO rooms (name) VALUES (?) RETURNING id, name" [paramName]
  resultsToMaybeRoom results

updateRoom :: Int -> String -> IO (Maybe Room)
updateRoom paramId paramName = do
  results <- withConn $ \conn -> try $ query conn "UPDATE rooms SET name = ? WHERE id = ? RETURNING id, name" (paramName, paramId)
  resultsToMaybeRoom results

deleteRoom :: Int -> IO (Maybe Room)
deleteRoom paramId = do
  results <- withConn $ \conn -> try $ query conn "DELETE FROM rooms WHERE id = ? RETURNING id, name" [paramId]
  resultsToMaybeRoom results

-- helper functions

resultsToMaybeRoom :: Either SqlError [(Integer, String)] -> IO (Maybe Room)
resultsToMaybeRoom maybeRoom = do
  case maybeRoom of
    Left err -> do
      print err
      return Nothing
    Right [(resId, resName)] -> do
      return $ Just $ Room { rId = resId, rName = resName }
    Right invalid -> do
      print invalid
      return Nothing

resultsToMaybeRoom' :: [(Integer, String)] -> IO (Maybe Room)
resultsToMaybeRoom' = \case
  [(resId, resName)] ->
    return $ Just $ Room { rId = resId, rName = resName }
  _ ->
    return Nothing