module Controllers.Drawers
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Drawer (Drawer, getAllDrawers, getDrawer, createDrawer, updateDrawer, deleteDrawer)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)

index :: ActionM ()
index = do
  paramStorageUnitId :: Integer <- param "storage_unit_id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllDrawers userId paramStorageUnitId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getDrawer userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramStorageUnitId :: Integer <- param "storage_unit_id"
  paramLevel :: Integer <- param "level"
  paramNote :: String <- param "note"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createDrawer userId paramStorageUnitId paramLevel paramNote) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramStorageUnitId :: Maybe Integer <- optionalIntegerParam "storage_unit_id"
  paramLevel :: Maybe Integer <- optionalIntegerParam "level"
  paramNote :: Maybe String <- optionalParam "note"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateDrawer userId paramId paramStorageUnitId paramLevel paramNote) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteDrawer userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Drawer -> ActionM ()
resultToJsonResponse = \case
  Nothing     -> json ()
  Just drawer -> json drawer
