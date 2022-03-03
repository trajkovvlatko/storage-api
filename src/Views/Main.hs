module Views.Main
  (
    indexView
  )
where

import GHC.Generics (Generic)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
  ( Html
  , div
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
      h3 "Storage"
      ul $ do
        li "index: GET /storage"
        li "show: GET /storage/:id"
        li "create: POST /storage"
        li "update: PATCH /storage/:id"
        li "delete: DELETE /storage/:id"
      h3 "Drawer"
      ul $ do
        li "index: GET /drawer"
        li "show: GET /drawer/:id"
        li "create: POST /drawer"
        li "update: PATCH /drawer/:id"
        li "delete: DELETE /drawer/:id"
