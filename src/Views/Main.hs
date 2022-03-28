module Views.Main
  ( indexView,
  )
where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
  ( div,
    h1,
    h3,
    li,
    p,
    ul,
    (!),
  )
import Text.Blaze.Html5.Attributes (class_)
import Views.Layout (layoutView)
import Web.Scotty (ActionM, html)
import Prelude hiding (div, head, id)

indexView :: ActionM ()
indexView = (html . renderHtml) $ do
  layoutView $ do
    div ! class_ "container" $ do
      h1 "Storage"
      p "API docs"
      h3 "Auth"
      ul $ do
        li "login: POST /login"
        li "register: POST /register"
      h3 "Rooms"
      ul $ do
        li "index: GET /rooms"
        li "show: GET /rooms/:id"
        li "create: POST /rooms"
        li "update: PATCH /rooms/:id"
        li "delete: DELETE /rooms/:id"
      h3 "Storage units"
      ul $ do
        li "index: GET /storage_units"
        li "show: GET /storage_units/:id"
        li "create: POST /storage_units"
        li "update: PATCH /storage_units/:id"
        li "delete: DELETE /storage_units/:id"
      h3 "Drawers"
      ul $ do
        li "index: GET /drawers"
        li "show: GET /drawers/:id"
        li "create: POST /drawers"
        li "update: PATCH /drawers/:id"
        li "delete: DELETE /drawers/:id"
      h3 "Colors"
      ul $ do
        li "index: GET /colors"
        li "show: GET /colors/:id"
        li "create: POST /colors"
        li "update: PATCH /colors/:id"
        li "delete: DELETE /colors/:id"
      h3 "Item types"
      ul $ do
        li "index: GET /item_types"
        li "show: GET /item_types/:id"
        li "create: POST /item_types"
        li "update: PATCH /item_types/:id"
        li "delete: DELETE /item_types/:id"
      h3 "Items"
      ul $ do
        li "index: GET /items"
        li "show: GET /items/:id"
        li "create: POST /items"
        li "update: PATCH /items/:id"
        li "delete: DELETE /items/:id"
