module Controllers.Main where

import Views.Main
  ( indexView,
  )
import Web.Scotty (ActionM)

index :: ActionM ()
index = indexView
