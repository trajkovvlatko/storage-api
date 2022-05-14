module Router
  ( routes,
  )
where

import qualified Controllers.Auth as Auth
import qualified Controllers.Colors as Colors
import qualified Controllers.Drawers as Drawers
import qualified Controllers.ItemTypes as ItemTypes
import qualified Controllers.Items as Items
import qualified Controllers.Main as Main
import qualified Controllers.Rooms as Rooms
import qualified Controllers.Search as Search
import qualified Controllers.StorageUnits as StorageUnits
import Network.Wai.Middleware.Cors
import Text.Blaze.Html4.FrameSet (col)
import Web.Scotty as S (ScottyM, addHeader, delete, get, middleware, options, patch, post, regex, text)

withCorsHeaders = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "GET, POST, PATCH, DELETE, OPTIONS"
  addHeader "Access-Control-Allow-Headers" "token, Content-Length, Access-Control-Allow-Origin, Access-Control-Allow-Headers, Access-Control-Allow-Methods, Access-Control-Allow-Credentials"

-- TODO: Create a proper middleware instead of chaining a function

routes :: ScottyM ()
routes = do
  options (regex "^/(.*)$") $ withCorsHeaders >> text "ok"
  --
  -- Root path
  get "/" Main.index

  -- Auth
  post "/login" $ withCorsHeaders >> Auth.login
  post "/register" $ withCorsHeaders >> Auth.register

  -- Rooms routes
  get "/rooms" $ withCorsHeaders >> Rooms.index
  get "/rooms/:id" $ withCorsHeaders >> Rooms.preview
  post "/rooms" $ withCorsHeaders >> Rooms.create
  patch "/rooms/:id" $ withCorsHeaders >> Rooms.update
  delete "/rooms/:id" $ withCorsHeaders >> Rooms.delete

  -- StorageUnits routes
  get "/storage_units" $ withCorsHeaders >> StorageUnits.index
  get "/storage_units/:id" $ withCorsHeaders >> StorageUnits.preview
  post "/storage_units" $ withCorsHeaders >> StorageUnits.create
  patch "/storage_units/:id" $ withCorsHeaders >> StorageUnits.update
  delete "/storage_units/:id" $ withCorsHeaders >> StorageUnits.delete

  -- Drawers routes
  get "/drawers" $ withCorsHeaders >> Drawers.index
  get "/drawers/:id" $ withCorsHeaders >> Drawers.preview
  post "/drawers" $ withCorsHeaders >> Drawers.create
  patch "/drawers/:id" $ withCorsHeaders >> Drawers.update
  delete "/drawers/:id" $ withCorsHeaders >> Drawers.delete

  -- ItemTypes routes
  get "/item_types" $ withCorsHeaders >> ItemTypes.index
  get "/item_types/:id" $ withCorsHeaders >> ItemTypes.preview
  post "/item_types" $ withCorsHeaders >> ItemTypes.create
  patch "/item_types/:id" $ withCorsHeaders >> ItemTypes.update
  delete "/item_types/:id" $ withCorsHeaders >> ItemTypes.delete

  -- Colors routes
  get "/colors" $ withCorsHeaders >> Colors.index
  get "/colors/:id" $ withCorsHeaders >> Colors.preview
  post "/colors" $ withCorsHeaders >> Colors.create
  patch "/colors/:id" $ withCorsHeaders >> Colors.update
  delete "/colors/:id" $ withCorsHeaders >> Colors.delete

  -- Items routes
  get "/items" $ withCorsHeaders >> Items.index
  get "/items/:id" $ withCorsHeaders >> Items.preview
  post "/items" $ withCorsHeaders >> Items.create
  patch "/items/:id" $ withCorsHeaders >> Items.update
  delete "/items/:id" $ withCorsHeaders >> Items.delete

  -- Search routes
  get "/search/basic/:term" $ withCorsHeaders >> Search.basic
