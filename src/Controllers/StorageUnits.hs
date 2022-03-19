module Controllers.StorageUnits
  ( index,
    preview,
    create,
    update,
    delete,
  )
where

import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Room (RoomId)
import Models.StorageUnit
  ( StorageUnit,
    StorageUnitId,
    StorageUnitName,
    createStorageUnit,
    deleteStorageUnit,
    getAllStorageUnits,
    getStorageUnit,
    updateStorageUnit,
  )
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  paramRoomId :: RoomId <- param "room_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllStorageUnits userId paramRoomId) >>= json

preview :: ActionM ()
preview = do
  paramId :: StorageUnitId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getStorageUnit userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: StorageUnitName <- param "name"
  paramRoomId :: RoomId <- param "room_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createStorageUnit userId paramRoomId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: StorageUnitId <- param "id"
  paramRoomId :: Maybe RoomId <- optionalIntegerParam "room_id"
  paramName :: Maybe StorageUnitName <- optionalParam "name"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateStorageUnit userId paramId paramRoomId paramName) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: StorageUnitId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteStorageUnit userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe StorageUnit -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just room -> json room
