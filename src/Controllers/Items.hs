module Controllers.Items
  ( index,
    preview,
    create,
    update,
    delete,
  )
where

import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Drawer (DrawerId)
import Models.Item (Item, ItemId, createItem, deleteItem, getAllItems, getItem, updateItem)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  paramDrawerId :: DrawerId <- param "drawer_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getAllItems userId paramDrawerId) >>= json

preview :: ActionM ()
preview = do
  paramId :: ItemId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (getItem userId paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramDrawerId :: DrawerId <- param "drawer_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (createItem userId paramDrawerId) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: ItemId <- param "id"
  paramDrawerId :: Maybe DrawerId <- optionalIntegerParam "drawer_id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (updateItem userId paramId paramDrawerId) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: ItemId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (deleteItem userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Item -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just item -> json item
