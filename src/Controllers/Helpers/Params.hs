module Controllers.Helpers.Params
  ( optionalParam,
    optionalIntegerParam,
  )
where

import ClassyPrelude (LText)
import Web.Scotty (ActionM, param, rescue)

optionalParam :: LText -> ActionM (Maybe String)
optionalParam p = (Just <$> param p) `rescue` const (return Nothing)

optionalIntegerParam :: LText -> ActionM (Maybe Integer)
optionalIntegerParam p = (Just <$> param p) `rescue` const (return Nothing)
