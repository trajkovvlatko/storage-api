module Controllers.Main where

import Views.Main
  (
    indexView
  )
import Web.Scotty ( ActionM, text )

index :: ActionM ()
index = indexView