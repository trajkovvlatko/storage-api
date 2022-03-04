module Views.Main
  (
    indexView
  )
where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
  ( div
  , h1
  , p
  , (!)
  , ul
  , li, h3
  )
import Text.Blaze.Html5.Attributes ( class_ )
import Prelude hiding ( div, head, id )
import Web.Scotty ( ActionM, html )
import Views.Layout (layoutView)

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
      h3 "Storage unit"
      ul $ do
        li "index: GET /storage_units"
        li "show: GET /storage_units/:id"
        li "create: POST /storage_units"
        li "update: PATCH /storage_units/:id"
        li "delete: DELETE /storage_units/:id"
      h3 "Drawer"
      ul $ do
        li "index: GET /drawers"
        li "show: GET /drawers/:id"
        li "create: POST /drawers"
        li "update: PATCH /drawers/:id"
        li "delete: DELETE /drawers/:id"
