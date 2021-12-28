module Views.Layout
  ( layoutView
  )
where

import Text.Blaze.Html (Html)
import Text.Blaze.Html5
  ( Html
  , body
  , docTypeHtml
  , head
  , meta
  , (!)
  )
import Text.Blaze.Html5.Attributes ( charset )
import Web.Scotty ( ActionM )
import Prelude hiding ( div, head, id )

layoutView :: Html -> Html
layoutView view = docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
  body $ do
    view