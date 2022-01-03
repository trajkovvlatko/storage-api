module Router
  ( routes
  )
where

import Web.Scotty as S ( get, post, ScottyM, patch, delete )
import qualified Controllers.Main as Main
import qualified Controllers.Storages as Storages
import qualified Controllers.Rooms as Rooms
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

  -- Storage routes
  get "/storages" Storages.index
  get "/storages/:id" Storages.preview