module Controllers.Colors
  ( index,
    preview,
    create,
    update,
    delete,
  )
where

import Controllers.Helpers.Params (optionalIntegerParam, optionalParam)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Color (Color, ColorId, createColor, deleteColor, getAllColors, getColor, updateColor)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

index :: ActionM ()
index = do
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO getAllColors >>= json

preview :: ActionM ()
preview = do
  paramId :: ColorId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (getColor paramId) >>= resultToJsonResponse

create :: ActionM ()
create = do
  paramLabel :: String <- param "label"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (createColor paramLabel) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: ColorId <- param "id"
  paramLabel :: Maybe String <- optionalParam "label"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (updateColor paramId paramLabel) >>= resultToJsonResponse

delete :: ActionM ()
delete = do
  paramId :: ColorId <- param "id"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right _ -> liftAndCatchIO (deleteColor paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Color -> ActionM ()
resultToJsonResponse = \case
  Nothing -> json ()
  Just item -> json item
