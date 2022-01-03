module Controllers.Auth 
  ( login
  , register
  , logout
  ) where
import Web.Scotty (ActionM, text, liftAndCatchIO, param, json)
import Lib.Auth (userIdToToken, tokenToUserId)
import Data.Text.Lazy (pack, Text)
import Models.User (createUser)

login :: ActionM ()
login = text "login"

register :: ActionM ()
register = do
  paramEmail :: String <- param "email"
  paramPassword :: String <- param "password"
  maybeUser <- liftAndCatchIO (createUser paramEmail paramPassword)
  case maybeUser of
    Nothing   -> json () -- show an error message here
    Just user -> json user

-- login :: ActionM ()
-- login = do
--   encoded <- liftAndCatchIO $ userIdToToken 1234
--   case encoded of
--     Left _ -> text "Cannot encode userId to token"
--     Right encodedUserId -> do
--       text $ (pack . show) encodedUserId

-- register :: ActionM ()
-- register = do
--   decoded <- liftAndCatchIO $ tokenToUserId "eyJraWQiOiJkZXZlbG9wbWVudCIsImFsZyI6IlJTMjU2In0.MTIzNA.CDXtgbti0WLXn9AAAcGQLd8YUxMos1_8OIZj5qriE7595Za73tsb9B-7-zB-KDlAQhm5bp-W2LeS6oiepK32JupUBFeC21-24ODnlL2VZKcJpP1p7FTiwJCCjiFh65v8m-VC79IT0dCFeux9rNvQ5AsXCxHNfgLRZJFEo2iC_uaGPk30Ln-d5v0LtIYVU5ZVD-oEyyVYbSPGFt5ifu4RwdEn6NSChx-1vY2sGcWr19rKnIDeg-455O_Z4bBZ6_JJPTfmnpqf0N0dquqc-GvN9zodHUYJoUb3wFUWkj0XFbMHC9ZIapZTRfOqt0A-h79caik1HejBq-wOp9HIyHCJaw"
--   case decoded of
--     Left _ -> text "Cannot decode token"
--     Right userId -> text $ integerToText userId

logout :: ActionM ()
logout = do
  text "logout"

-- helper functions

integerToText :: Integer -> Text
integerToText = pack . show