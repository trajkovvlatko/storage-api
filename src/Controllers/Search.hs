module Controllers.Search
  ( basic,
  )
where

import Lib.Auth (invalidTokenJSONResponse, withUserIdOrErr)
import Models.Item (Term, basicSearch)
import Web.Scotty (ActionM, json, liftAndCatchIO, param)

basic :: ActionM ()
basic = do
  paramTerm :: Term <- param "term"
  withUserIdOrErr >>= \case
    Left err -> invalidTokenJSONResponse err
    Right userId -> liftAndCatchIO (basicSearch userId paramTerm) >>= json
