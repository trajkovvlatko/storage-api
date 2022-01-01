module Controllers.Rooms
  (
    index
  , preview
  ) where

import qualified Web.Scotty as S
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs )
import Data.Pool (withResource)
import Database (withConn)
import Database.PostgreSQL.Simple ( execute
                                  , query_
                                  , Only (..)
                                  , FromRow )
import ClassyPrelude (IsSet(setFromList))
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Web.Scotty (liftAndCatchIO)

data Room = Room
  { rId     :: Int
  , rName   :: String } deriving Generic

instance ToJSON Room where
  toEncoding (Room rId rName) =
    pairs $    "id"   .= rId
            <> "name" .= rName

instance FromRow Room where
  fromRow = Room <$> field <*> field

index :: S.ActionM ()
index = do
  result <- liftAndCatchIO allRooms
  case result of
    [] -> S.text "Empty"
    arr -> S.json $ map (\x -> Room { rId = rId x, rName = rName x }) arr

allRooms :: IO [Room]
allRooms = withConn $ \conn -> query_ conn "SELECT id, name FROM rooms;"

preview :: S.ActionM ()
preview = do
  paramId :: Int <- S.param "id"
  S.json $ Room { rId = paramId, rName = "name" }

create :: S.ActionM ()
create = do
  paramId :: Int <- S.param "id"
  S.json $ Room { rId = paramId, rName = "name" }

update :: S.ActionM ()
update = do
  paramId :: Int <- S.param "id"
  S.json $ Room { rId = paramId, rName = "name" }

delete :: S.ActionM ()
delete = do
  paramId :: Int <- S.param "id"
  S.json $ Room { rId = paramId, rName = "name" }