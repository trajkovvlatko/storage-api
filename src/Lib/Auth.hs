{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib.Auth
  ( encodeUserIdToToken
  , invalidTokenJSONResponse
  , withUserIdOrErr
  , UserId
  ) where

import GHC.Generics (Generic)
import Web.Scotty (liftAndCatchIO, ActionM, json, param, header)
import ClassyPrelude (Utf8 (decodeUtf8), encodeUtf8, readFile, ByteString, Text, pack, LazySequence (toStrict, fromStrict), tshow, MonadIO (liftIO), first)
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Data.Aeson as Aeson
import qualified System.Environment as ENV
import Jose.Jwe
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt
import Data.Text.Read (decimal, Reader)
import Lib.Error (ErrorResponse(ErrorResponse, eMessage))
import Models.User (User (uId))
import Data.Aeson (ToJSON(toJSON, toEncoding), KeyValue((.=)), pairs, FromJSON)
import Prelude hiding (readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- import Data.Time.Clock.POSIX (getPOSIXTime)

type UserId = Integer
type Token = Text

data AuthError
  = TokenDecodeError
  | TokenEncodeError
  | TokenMissingError
  deriving (Eq, Show)

newtype TokenResponse = TokenResponse { uToken :: Text } deriving Generic

data TokenValue = TokenValue { sub :: String
                             , exp :: Integer 
                             } deriving (Show, Generic)

instance FromJSON TokenValue

instance ToJSON TokenResponse where
  toEncoding (TokenResponse uToken) =
    pairs $ "token" .= uToken

getJwks :: IO [Jwk]
getJwks = do
  jwkSig <- readFile ".jwk.sig"
  let parsedJwkSig = Aeson.eitherDecodeStrict jwkSig
  return $ either (\e -> error "Error: Cannot parse .jwk.sig.") pure parsedJwkSig

tokenToUserIdOrErr :: Token -> IO (Either AuthError UserId)
tokenToUserIdOrErr token = do
  jwks <- getJwks
  decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (encodeUtf8 token)
  case decoded of
    Left _              -> return $ Left TokenDecodeError
    Right (Jws (_, bs)) -> do
      let claims = decodeUtf8 bs
      let strict = fromStrict bs
      let decodedClaims = Aeson.eitherDecode strict :: Either String TokenValue
      case decodedClaims of
        Left s -> return $ Left TokenDecodeError
        Right tv -> return $ Right $ readInt $ pack $ sub tv
    _ -> return $ Left TokenDecodeError

userIdToTokenOrErr :: UserId -> ActionM (Either AuthError Token)
userIdToTokenOrErr userId = do
  liftAndCatchIO $ do
    jwks <- getJwks
    -- let bsUserId = (pack . encodeUtf8) $ show userId
    curTime <- liftIO getPOSIXTime
    let claim = JwtClaims { jwtIss = Nothing
                          , jwtSub = Just $ tshow userId
                          , jwtAud = Nothing
                          , jwtExp = Just $ IntDate $ curTime + 10
                          , jwtNbf = Nothing
                          , jwtIat = Nothing
                          , jwtJti = Nothing
                          }
    encoded <- Jose.Jwt.encode jwks (JwsEncoding RS256) (Claims . toStrict . Aeson.encode $ claim)
    case encoded of
      Left je   -> return $ Left TokenEncodeError
      Right jwt -> return $ Right (getValueFromJwt jwt)

encodeUserIdToToken :: UserId -> ActionM ()
encodeUserIdToToken userId = do
  userIdToTokenOrErr userId >>= \case
    Left _              -> json $ ErrorResponse { eMessage = "Cannot encode user token." }
    Right encodedUserId -> json $ TokenResponse { uToken = encodedUserId }

getValueFromJwt :: Jwt -> Text
getValueFromJwt = decodeUtf8 . unJwt

getIntFromText :: Reader a -> Text -> a
getIntFromText = (value .)
  where value (Right (v,_)) = v

readInt :: Text -> Integer
readInt = getIntFromText decimal

withUserIdOrErr :: ActionM (Either AuthError UserId)
withUserIdOrErr = do
  token <- header "token"
  case token of
    Nothing -> return $ Left TokenMissingError
    Just txt -> liftAndCatchIO $ tokenToUserIdOrErr (toStrict txt)

invalidTokenJSONResponse :: AuthError -> ActionM ()
invalidTokenJSONResponse err = json $ case err of
  TokenMissingError -> ErrorResponse { eMessage = "Invalid user token." }
  TokenEncodeError -> ErrorResponse { eMessage = "Cannot encode token." }
  TokenDecodeError -> ErrorResponse { eMessage = "Cannot decode token." }