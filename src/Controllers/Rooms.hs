module Controllers.Rooms
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Room (Room, getAllRooms, getRoom, createRoom, updateRoom, deleteRoom)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)

index :: ActionM ()
index = do
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllRooms userId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getRoom userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: String <- param "name"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createRoom userId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramName :: String <- param "name"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateRoom userId paramId paramName) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteRoom userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Room -> ActionM ()
resultToJsonResponse = \case
  Nothing   -> json ()
  Just room -> json room
