module Models.Room 
  ( getAllRooms
  , Room
    ( Room
    , rId
    , rName)
  ) where

import GHC.Generics (Generic)
import Database (withConn)
import Database.PostgreSQL.Simple ( query_ )
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

getAllRooms :: IO [Room]
getAllRooms = withConn $ \conn -> query_ conn "SELECT id, name FROM rooms;"