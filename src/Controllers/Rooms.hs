module Controllers.Rooms
  (
    index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Room (Room (Room, rId, rName), getAllRooms)

index :: ActionM ()
index = liftAndCatchIO getAllRooms >>= json

preview :: ActionM ()
preview = do
  paramId :: Int <- param "id"
  json $ Room { rId = paramId, rName = "name" }

create :: ActionM ()
create = do
  paramId :: Int <- param "id"
  json $ Room { rId = paramId, rName = "name" }

update :: ActionM ()
update = do
  paramId :: Int <- param "id"
  json $ Room { rId = paramId, rName = "name" }

delete :: ActionM ()
delete = do
  paramId :: Int <- param "id"
  json $ Room { rId = paramId, rName = "name" }