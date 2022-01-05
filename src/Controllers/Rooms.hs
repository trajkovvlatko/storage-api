module Controllers.Rooms
  ( index
  , preview
  , create
  , update
  , delete
  ) where

import Web.Scotty (liftAndCatchIO, ActionM, json, param)
import Models.Room (Room (Room, rId, rName), getAllRooms, getRoom, createRoom, updateRoom, deleteRoom)
import Lib.Auth (tokenToUserId)
import ClassyPrelude (unpack, pack)
import Lib.Error (ErrorResponse(ErrorResponse, eMessage))

index :: ActionM ()
index = do
  token :: String <- param "token"
  userIdOrErr <- liftAndCatchIO $ tokenToUserId (pack token)
  case userIdOrErr of
    Left _       -> json $ ErrorResponse { eMessage = "Invalid user token." }
    Right userId -> liftAndCatchIO (getAllRooms userId) >>= json

preview :: ActionM ()
preview = do
  paramId :: Integer <- param "id"
  token :: String <- param "token"
  userIdOrErr <- liftAndCatchIO $ tokenToUserId (pack token)
  case userIdOrErr of
    Left _       -> json ErrorResponse { eMessage = "Invalid user token." }
    Right userId -> liftAndCatchIO (getRoom userId paramId) >>= resultToJsonResponse
  
create :: ActionM ()
create = do
  paramName :: String <- param "name"
  token :: String <- param "token"
  userIdOrErr <- liftAndCatchIO $ tokenToUserId (pack token)
  case userIdOrErr of
    Left _       -> json ErrorResponse { eMessage = "Invalid user token." }
    Right userId -> liftAndCatchIO (createRoom userId paramName) >>= resultToJsonResponse

update :: ActionM ()
update = do
  paramId :: Integer <- param "id"
  paramName :: String <- param "name"
  token :: String <- param "token"
  userIdOrErr <- liftAndCatchIO $ tokenToUserId (pack token)
  case userIdOrErr of
    Left _       -> json ErrorResponse { eMessage = "Invalid user token." }
    Right userId -> liftAndCatchIO (updateRoom userId paramId paramName) >>= resultToJsonResponse
  

delete :: ActionM ()
delete = do
  paramId :: Integer <- param "id"
  token :: String <- param "token"
  userIdOrErr <- liftAndCatchIO $ tokenToUserId (pack token)
  case userIdOrErr of
    Left _       -> json ErrorResponse { eMessage = "Invalid user token." }
    Right userId -> liftAndCatchIO (deleteRoom userId paramId) >>= resultToJsonResponse

-- helper functions

resultToJsonResponse :: Maybe Room -> ActionM ()
resultToJsonResponse = \case
  Nothing   -> json ()
  Just room -> json room