module Controllers.Rooms
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Room (Room (Room, rId, rName), getAllRooms, getRoom, createRoom, updateRoom, deleteRoom)
import Lib.Auth (tokenToUserId, AuthError, UserId, invalidTokenJSONResponse, withUserIdOrErr)
import ClassyPrelude (unpack, pack)
import Lib.Error (ErrorResponse(ErrorResponse, eMessage))

index :: ActionM ()
index = do
  withUserIdOrErr >>= \case
    Left _       -> invalidTokenJSONResponse
    Right userId -> liftAndCatchIO (getAllRooms userId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left _       -> invalidTokenJSONResponse
    Right userId -> liftAndCatchIO (getRoom userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: String <- param "name"
  withUserIdOrErr >>= \case
    Left _       -> invalidTokenJSONResponse
    Right userId -> liftAndCatchIO (createRoom userId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramName :: String <- param "name"
  withUserIdOrErr >>= \case
    Left _       -> invalidTokenJSONResponse
    Right userId -> liftAndCatchIO (updateRoom userId paramId paramName) >>= resultToJsonResponse


delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left _       -> invalidTokenJSONResponse
    Right userId -> liftAndCatchIO (deleteRoom userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Room -> ActionM ()
resultToJsonResponse = \case
  Nothing   -> json ()
  Just room -> json room