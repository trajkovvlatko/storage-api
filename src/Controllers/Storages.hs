module Controllers.Storages
  (
    index
  , preview
  , create
  ) where
import qualified Web.Scotty as S
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs )

data Storage = Storage
  { sId     :: Int
  , sRoomId :: Int
  , sName   :: String } deriving Generic

instance ToJSON Storage where
  toEncoding (Storage sId sRoomId sName) =
    pairs $    "id" .= sId
            <> "room_id" .= sRoomId
            <> "name" .= sName

index :: S.ActionM ()
index = do
  S.text "controllers.storages.index"

preview :: S.ActionM ()
preview = do
  paramId :: Int <- S.param "id"
  S.json $ Storage { sId = paramId, sRoomId = 123, sName = "name" }

create :: S.ActionM ()
create = do
  paramId :: Int <- S.param "id"
  S.json $ Storage { sId = paramId, sRoomId = 123, sName = "name" }

update :: S.ActionM ()
update = do
  paramId :: Int <- S.param "id"
  S.json $ Storage { sId = paramId, sRoomId = 123, sName = "name" }

delete :: S.ActionM ()
delete = do
  paramId :: Int <- S.param "id"
  S.json $ Storage { sId = paramId, sRoomId = 123, sName = "name" }