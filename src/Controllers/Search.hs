module Controllers.Search
  ( basic,
  )
where

import Data.Char
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Item (Term, basicSearch)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

basic :: ActionM ()
basic = do
  paramTerm :: Term <- param "term"
  liftAndCatchIO $ print (clearString paramTerm)
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO results >>= json
      where
        clearedTerm = clearString paramTerm
        results = basicSearch userId clearedTerm

-- helpers

clearString :: Term -> Term
clearString string = unwords $ words $ filter (\c -> isAlpha c || isSpace c) $ map toLower string
