module Controllers.Search
  ( basic,
  )
where

import Controllers.Helpers.Params (clearString)
import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Item (Term, basicSearch)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

basic :: ActionM ()
basic = do
  paramTerm :: Term <- param "term"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO results >>= json
      where
        clearedTerm = clearString paramTerm
        results = basicSearch userId clearedTerm
