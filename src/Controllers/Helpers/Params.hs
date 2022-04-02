module Controllers.Helpers.Params
  ( optionalParam,
    optionalIntegerParam,
    clearString,
  )
where

import ClassyPrelude (LText)
import Data.Char
import Models.Item (Term)
import Web.Scotty (ActionM, param, rescue)

optionalParam :: LText -> ActionM (Maybe String)
optionalParam p = (Just <$> param p) `rescue` const (return Nothing)

optionalIntegerParam :: LText -> ActionM (Maybe Integer)
optionalIntegerParam p = (Just <$> param p) `rescue` const (return Nothing)

clearString :: Term -> Term
clearString string = unwords $ words $ filter (\c -> isAlpha c || isSpace c) $ map toLower string
