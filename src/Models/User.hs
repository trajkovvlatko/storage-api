{-# OPTIONS_GHC -Wno-missing-fields #-}

module Models.User 
  ( createUser
  , User
  , uId
  , uName
  ) where

import Database (withConn)
import Database.PostgreSQL.Simple (query, FromRow)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Aeson (ToJSON(toJSON, toEncoding), object, KeyValue((.=)), pairs)
import Data.Password.Bcrypt (hashPassword, mkPassword, PasswordHash (unPasswordHash))
import ClassyPrelude (unpack, pack)

data User = User
  { uId       :: Integer
  , uName     :: String
  , uPassword :: String } deriving Generic

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToJSON User where
  toEncoding (User uId uName rPassword) =
    pairs $    "id"   .= uId
            <> "name" .= uName

createUser :: String -> String -> IO (Maybe User)
createUser paramEmail paramPassword = do
  hashedPassword <- getHashedPassword paramPassword
  results <- withConn $ \conn -> query conn queryString [paramEmail, hashedPassword]
  resultsToMaybeUser results
  where
    queryString = "INSERT INTO users (email, password) VALUES (?, ?) RETURNING id, email"

-- helper functions
getHashedPassword :: String -> IO String
getHashedPassword input = do
  hashedPasswordObject <- hashPassword $ mkPassword (pack input)
  return $ (unpack . unPasswordHash) hashedPasswordObject

resultsToMaybeUser :: [(Integer, String)] -> IO (Maybe User)
resultsToMaybeUser = \case
  [(resId, resName)] ->
    return $ Just $ User { uId = resId, uName = resName }
  _ ->
    return Nothing