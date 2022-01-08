module Controllers.Drawers
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param, params)
import Models.Drawer (Drawer (Drawer, dId, dLevel, dNote), getAllDrawers, getDrawer, createDrawer, updateDrawer, deleteDrawer)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)

index :: ActionM ()
index = do
  paramStorageId :: Integer <- param "storage_id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllDrawers userId paramStorageId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getDrawer userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  params <- params
  liftAndCatchIO (print params)
  paramStorageId :: Integer <- param "storage_id"
  paramLevel :: Integer <- param "level"
  paramNote :: String <- param "name"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createDrawer userId paramStorageId paramLevel paramNote) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramStorageId :: Integer <- param "storage_id"
  paramLevel :: Integer <- param "level"
  paramNote :: String <- param "name"
  withUserIdOrErr >>= \case
    Left err     -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateDrawer userId paramId paramStorageId paramLevel paramNote) >>= resultToJsonResponse

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