module Models.Room
  ( getAllRooms,
    getRoom,
    createRoom,
    updateRoom,
    deleteRoom,
    RoomId,
    RoomName,
    Room
      ( Room,
        rId,
        rName
      ),
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Database (withConn)
import Database.PostgreSQL.Simple (Only (Only), query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics (Generic)
import Lib.Auth (UserId)

type RoomId = Integer

type RoomName = String

data Room = Room
  { rId :: RoomId,
    rUserId :: UserId,
    rName :: RoomName
  }
  deriving (Generic)

instance FromRow Room where
  fromRow = Room <$> field <*> field <*> field

instance ToJSON Room where
  toEncoding (Room id' userId name) =
    pairs $
      "id" .= id'
        <> "user_id" .= userId
        <> "name" .= name

-- queries

getAllRooms :: UserId -> IO [Room]
getAllRooms userId = do
  withConn $ \conn -> query conn queryString (Only userId)
  where
    queryString = [sql| SELECT id, user_id, name FROM rooms WHERE user_id = ? |]

getRoom :: UserId -> RoomId -> IO (Maybe Room)
getRoom userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeRoom
  where
    queryString = [sql| SELECT id, user_id, name FROM rooms WHERE id = ? AND user_id = ? LIMIT 1 |]

createRoom :: UserId -> RoomName -> IO (Maybe Room)
createRoom userId paramName = do
  withConn $ \conn -> query conn queryString (paramName, userId) >>= resultsToMaybeRoom
  where
    queryString = [sql| INSERT INTO rooms (name, user_id) VALUES (?, ?) RETURNING id, user_id, name |]

updateRoom :: UserId -> RoomId -> RoomName -> IO (Maybe Room)
updateRoom userId paramId paramName = do
  withConn $ \conn -> query conn queryString (paramName, paramId, userId) >>= resultsToMaybeRoom
  where
    queryString = [sql| UPDATE rooms SET name = ? WHERE id = ? AND user_id = ? RETURNING id, user_id, name |]

deleteRoom :: UserId -> RoomId -> IO (Maybe Room)
deleteRoom userId paramId = do
  withConn $ \conn -> query conn queryString (paramId, userId) >>= resultsToMaybeRoom
  where
    queryString = [sql| DELETE FROM rooms WHERE id = ? AND user_id = ? RETURNING id, user_id, name |]

-- helper functions

resultsToMaybeRoom :: [(RoomId, UserId, RoomName)] -> IO (Maybe Room)
resultsToMaybeRoom = \case
  [(resId, resUserId, resName)] -> return $ Just $ Room {rId = resId, rUserId = resUserId, rName = resName}
  _ -> return Nothing
