module Controllers.Rooms
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Room (Room (Room, rId, rName), getAllRooms, getRoom, createRoom, updateRoom, deleteRoom)

index :: ActionM ()
index = liftAndCatchIO getAllRooms >>= json

preview :: ActionM ()
preview = do
  paramId :: Int <- param "id"
  liftAndCatchIO (getRoom paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: String <- param "name"
  liftAndCatchIO (createRoom paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Int <- param "id"
  paramName :: String <- param "name"
  liftAndCatchIO (updateRoom paramId paramName) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: Int <- param "id"
  liftAndCatchIO (deleteRoom paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Room -> ActionM ()
resultToJsonResponse = \case
  Nothing   -> json ()
  Just room -> json room