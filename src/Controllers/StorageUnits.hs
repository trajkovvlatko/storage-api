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
import Models.StorageUnit
  ( StorageUnit,
    createStorageUnit,
    deleteStorageUnit,
    getAllStorageUnits,
    getStorageUnit,
    updateStorageUnit,
  )
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  paramRoomId :: Integer <- param "room_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllStorageUnits userId paramRoomId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getStorageUnit userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramName :: String <- param "name"
  paramRoomId :: Integer <- param "room_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createStorageUnit userId paramRoomId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramRoomId :: Maybe Integer <- optionalIntegerParam "room_id"
  paramName :: Maybe String <- optionalParam "name"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateStorageUnit userId paramId paramRoomId paramName) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteStorageUnit userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe StorageUnit -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just room -> json room
