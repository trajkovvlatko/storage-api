module Controllers.ItemTypes
  ( index,
    preview,
    create,
    update,
    delete,
  )
where

import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.ItemType (ItemType, ItemTypeId, createItemType, deleteItemType, getAllItemTypes, getItemType, updateItemType)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO getAllItemTypes >>= json

preview :: ActionM ()
preview = do
  paramId :: ItemTypeId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (getItemType paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramLabel :: String <- param "label"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (createItemType paramLabel) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: ItemTypeId <- param "id"
  paramLabel :: Maybe String <- optionalParam "label"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (updateItemType paramId paramLabel) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: ItemTypeId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (deleteItemType paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe ItemType -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just item -> json item
