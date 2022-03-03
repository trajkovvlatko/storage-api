module Controllers.Storages
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Storage (Storage (Storage, sId, sName), getAllStorages, getStorage, createStorage, updateStorage, deleteStorage)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Web.Scotty.Trans (ActionT)
import ClassyPrelude (Text)
import Controllers.Helpers.Params (optionalParam, optionalIntegerParam)

index :: ActionM ()
index = do
  paramRoomId :: Integer <- param "room_id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllStorages userId paramRoomId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getStorage userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: String <- param "name"
  paramRoomId :: Integer <- param "room_id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createStorage userId paramRoomId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramRoomId :: Maybe Integer <- optionalIntegerParam "room_id"
  paramName :: Maybe String <- optionalParam "name"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateStorage userId paramId paramRoomId paramName) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteStorage userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Storage -> ActionM ()
resultToJsonResponse = \case
  Nothing   -> json ()
  Just room -> json room
