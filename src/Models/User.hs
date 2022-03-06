module Models.User
  ( createUser
  , findUserByEmail
  , User
  , uId
  , uEmail
  , uPassword
  ) where

import Database (withConn)
import Database.PostgreSQL.Simple (query, FromRow, SqlError)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toEncoding), KeyValue((.=)), pairs)
import Control.Exception (try)
import ClassyPrelude (Text, unpack, IsString (fromString))
import Data.Password.Bcrypt (hashPassword, mkPassword, PasswordHash (unPasswordHash), Bcrypt)

data User = User
  { uId       :: Integer
  , uEmail    :: String
  , uPassword :: String } deriving Generic

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToJSON User where
  toEncoding (User id' email _) =
    pairs $    "id"    .= id'
            <> "email" .= email

getPasswordHash :: String -> IO (PasswordHash Bcrypt)
getPasswordHash password = hashPassword (mkPassword (fromString password))

createUser :: String -> String -> IO (Maybe User)
createUser paramEmail paramPassword = do
  passwordHash <- getPasswordHash paramPassword
  let passwordString = unpack $ unPasswordHash passwordHash
  resultsToMaybeUser =<< withConn (\conn -> try $ query conn queryString [paramEmail, passwordString])
  where
    queryString = "INSERT INTO users (email, password) VALUES (?, ?) RETURNING id, email, password"

findUserByEmail :: String  -> IO (Maybe User)
findUserByEmail paramEmail = do
  resultsToMaybeUser =<< withConn (\conn -> try $ query conn queryString [paramEmail])
  where
    queryString = "SELECT id, email, password FROM users WHERE email = ?"

-- helper functions

resultsToMaybeUser :: Either SqlError [(Integer, String, String)] -> IO (Maybe User)
resultsToMaybeUser maybeUser = do
  case maybeUser of
    Left err -> do
      print err
      return Nothing
    Right [(resId, resEmail, resPassword)] -> do
      return $ Just $ User { uId = resId, uEmail = resEmail, uPassword = resPassword }
    Right invalid -> do
      print invalid
      return Nothing
