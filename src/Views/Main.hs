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
      p "Lorem ipsum"