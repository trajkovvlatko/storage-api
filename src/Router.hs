module Router
  ( routes
  )
where

import Web.Scotty as S ( get, post, ScottyM, patch, delete )
import qualified Controllers.Main as Main
import qualified Controllers.StorageUnits as StorageUnits
import qualified Controllers.Rooms as Rooms
import qualified Controllers.Drawers as Drawers
import qualified Controllers.Auth as Auth

routes :: ScottyM ()
routes = do
  -- Root path
  get "/" Main.index

  -- Auth
  post "/login"    Auth.login
  post "/register" Auth.register

  -- Rooms routes
  get    "/rooms"     Rooms.index
  get    "/rooms/:id" Rooms.preview
  post   "/rooms"     Rooms.create
  patch  "/rooms/:id" Rooms.update
  delete "/rooms/:id" Rooms.delete

  -- StorageUnits routes
  get    "/storage_units"     StorageUnits.index
  get    "/storage_units/:id" StorageUnits.preview
  post   "/storage_units"     StorageUnits.create
  patch  "/storage_units/:id" StorageUnits.update
  delete "/storage_units/:id" StorageUnits.delete

  -- Drawer routes
  get    "/drawers"     Drawers.index
  get    "/drawers/:id" Drawers.preview
  post   "/drawers"     Drawers.create
  patch  "/drawers/:id" Drawers.update
  delete "/drawers/:id" Drawers.delete
