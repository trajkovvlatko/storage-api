module Controllers.Rooms
  (
    index
  , preview
  ) where

import qualified Web.Scotty as S
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs )

data Room = Room
  { rId     :: Int
  , rName   :: String } deriving Generic

instance ToJSON Room where
  toEncoding (Room rId rName) =
    pairs $    "id"   .= rId
            <> "name" .= rName

index :: S.ActionM ()
index = do
  S.text "controllers.rooms.index"

preview :: S.ActionM ()
preview = do
  paramId :: Int <- S.param "id"
  S.json $ Room { rId = paramId, rName = "name" }