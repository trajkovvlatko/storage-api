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
      h3 "Storage unit"
      ul $ do
        li "index: GET /rooms/:room_id/storage_units"
        li "show: GET /storage_units/:id"
        li "create: POST /rooms/:room_id/storage_units"
        li "update: PATCH /storage_units/:id"
        li "delete: DELETE /storage_units/:id"
      h3 "Drawer"
      ul $ do
        li "index: GET /storage_units/:storage_unit_id/drawers"
        li "show: GET /drawers/:id"
        li "create: POST /storage_units/:storage_unit_id/drawers"
        li "update: PATCH /drawers/:id"
        li "delete: DELETE /drawers/:id"
