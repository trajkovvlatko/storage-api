module Controllers.Drawers
  ( index,
    preview,
    create,
    update,
    delete,
  )
where

import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Drawer (Drawer, DrawerId, DrawerLevel, DrawerNote, createDrawer, deleteDrawer, getAllDrawers, getDrawer, updateDrawer)
import Models.StorageUnit (StorageUnitId)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  paramStorageUnitId :: StorageUnitId <- param "storage_unit_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllDrawers userId paramStorageUnitId) >>= json

preview :: ActionM ()
preview = do
  paramId :: DrawerId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getDrawer userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramStorageUnitId :: StorageUnitId <- param "storage_unit_id"
  paramLevel :: DrawerLevel <- param "level"
  paramNote :: DrawerNote <- param "note"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createDrawer userId paramStorageUnitId paramLevel paramNote) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: DrawerId <- param "id"
  paramStorageUnitId :: Maybe StorageUnitId <- optionalIntegerParam "storage_unit_id"
  paramLevel :: Maybe DrawerLevel <- optionalIntegerParam "level"
  paramNote :: Maybe DrawerNote <- optionalParam "note"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateDrawer userId paramId paramStorageUnitId paramLevel paramNote) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: DrawerId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteDrawer userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Drawer -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just drawer -> json drawer
