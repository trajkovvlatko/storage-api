module Router
  ( routes,
  )
where

import qualified Controllers.Auth as Auth
import qualified Controllers.Colors as Colors
import qualified Controllers.Drawers as Drawers
import qualified Controllers.ItemTypes as ItemTypes
import qualified Controllers.Main as Main
import qualified Controllers.Rooms as Rooms
import qualified Controllers.StorageUnits as StorageUnits
import Text.Blaze.Html4.FrameSet (col)
import Web.Scotty as S (ScottyM, delete, get, patch, post)

routes :: ScottyM ()
routes = do
  -- Root path
  get "/" Main.index

  -- Auth
  post "/login" Auth.login
  post "/register" Auth.register

  -- Rooms routes
  get "/rooms" Rooms.index
  get "/rooms/:id" Rooms.preview
  post "/rooms" Rooms.create
  patch "/rooms/:id" Rooms.update
  delete "/rooms/:id" Rooms.delete

  -- StorageUnits routes
  get "/rooms/:room_id/storage_units" StorageUnits.index
  get "/storage_units/:id" StorageUnits.preview
  post "/rooms/:room_id/storage_units" StorageUnits.create
  patch "/storage_units/:id" StorageUnits.update
  delete "/storage_units/:id" StorageUnits.delete

  -- Drawers routes
  get "/storage_units/:storage_unit_id/drawers" Drawers.index
  get "/drawers/:id" Drawers.preview
  post "/storage_units/:storage_unit_id/drawers" Drawers.create
  patch "/drawers/:id" Drawers.update
  delete "/drawers/:id" Drawers.delete

  -- ItemTypes routes
  get "/item_types" ItemTypes.index
  get "/item_types/:id" ItemTypes.preview
  post "/item_types" ItemTypes.create
  patch "/item_types/:id" ItemTypes.update
  delete "/item_types/:id" ItemTypes.delete

  -- Colors routes
  get "/colors" Colors.index
  get "/colors/:id" Colors.preview
  post "/colors" Colors.create
  patch "/colors/:id" Colors.update
  delete "/colors/:id" Colors.delete
