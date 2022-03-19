module Lib.Auth
  ( encodeUserIdToToken
  , invalidTokenJSONResponse
  , withUserIdOrErr
  , UserId
  ) where

import GHC.Generics (Generic)
import Web.Scotty (liftAndCatchIO, ActionM, json, header, status)
import ClassyPrelude (Utf8 (decodeUtf8), encodeUtf8, readFile, ByteString, Text, pack, LazySequence (toStrict, fromStrict), tshow, MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt
import Data.Text.Read (decimal, Reader)
import Lib.Error (ErrorResponse(ErrorResponse, eMessage))
import Data.Aeson (ToJSON(toEncoding), KeyValue((.=)), pairs, FromJSON)
import Prelude hiding (exp, readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (status401)

type UserId = Integer
type Token = Text

data AuthError
  = TokenDecodeError
  | TokenEncodeError
  | TokenMissingError
  | ClaimDecodeError
  | TokenExpiredError
  deriving (Eq, Show)

newtype TokenResponse = TokenResponse { uToken :: Text } deriving Generic

data TokenValue = TokenValue { sub :: String
                             , exp :: Integer 
                             } deriving (Show, Generic)

instance FromJSON TokenValue

instance ToJSON TokenResponse where
  toEncoding (TokenResponse token) =
    pairs $ "token" .= token

getJwks :: IO [Jwk]
getJwks = do
  jwkSig <- readFile ".jwk.sig"
  let parsedJwkSig = Aeson.eitherDecodeStrict jwkSig
  return $ either (\_ -> error "Error: Cannot parse .jwk.sig.") pure parsedJwkSig

tokenToUserIdOrErr :: Token -> IO (Either AuthError UserId)
tokenToUserIdOrErr token = do
  jwks <- getJwks
  decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (encodeUtf8 token)
  case decoded of
    Left _ -> return $ Left TokenDecodeError
    Right (Jws (_, bs)) -> verifyJWTClaims bs
    _ -> return $ Left TokenDecodeError

userIdToTokenOrErr :: UserId -> ActionM (Either AuthError Token)
userIdToTokenOrErr userId = do
  liftAndCatchIO $ do
    jwks <- getJwks
    currentTime <- liftIO getPOSIXTime
    let claim = JwtClaims { jwtIss = Nothing
                          , jwtSub = Just $ tshow userId
                          , jwtAud = Nothing
                          , jwtExp = Just $ IntDate $ currentTime + 15000
                          , jwtNbf = Nothing
                          , jwtIat = Nothing
                          , jwtJti = Nothing
                          }
    encoded <- Jose.Jwt.encode jwks (JwsEncoding RS256) (Claims . toStrict . Aeson.encode $ claim)
    case encoded of
      Left  _   -> return $ Left TokenEncodeError
      Right jwt -> return $ Right (getValueFromJwt jwt)

encodeUserIdToToken :: UserId -> ActionM ()
encodeUserIdToToken userId = do
  userIdToTokenOrErr userId >>= \case
    Left _              -> json $ ErrorResponse { eMessage = "Cannot encode user token." }
    Right encodedUserId -> json $ TokenResponse { uToken = encodedUserId }

verifyJWTClaims :: ByteString -> IO (Either AuthError UserId)
verifyJWTClaims bs = do
  let claims = Aeson.eitherDecode (fromStrict bs) :: Either String TokenValue
  case claims of
    Left _           -> return $ Left ClaimDecodeError
    Right tokenValue -> verifyClaimExpire tokenValue

verifyClaimExpire :: TokenValue -> IO (Either AuthError UserId)
verifyClaimExpire tokenValue = do
  currentTime <- liftIO getPOSIXTime
  let expireTime = exp tokenValue
  if expireTime > round currentTime
    then return $ (Right . readInt . pack . sub) tokenValue
    else return $ Left TokenExpiredError

getValueFromJwt :: Jwt -> Text
getValueFromJwt = decodeUtf8 . unJwt

readInt :: Text -> Integer
readInt = value . decimal
  where value (Right (v, k)) = v
        value (Left _) = -1

withUserIdOrErr :: ActionM (Either AuthError UserId)
withUserIdOrErr = do
  token <- header "token"
  case token of
    Nothing -> return $ Left TokenMissingError
    Just txt -> liftAndCatchIO $ tokenToUserIdOrErr (toStrict txt)

invalidTokenJSONResponse :: AuthError -> ActionM ()
invalidTokenJSONResponse err = status status401 >> json message
  where
    message = case err of
      TokenMissingError -> ErrorResponse { eMessage = "Invalid user token." }
      TokenEncodeError -> ErrorResponse { eMessage = "Cannot encode token." }
      TokenDecodeError -> ErrorResponse { eMessage = "Cannot decode token." }
      ClaimDecodeError -> ErrorResponse { eMessage = "Cannot decode claim." }
      TokenExpiredError -> ErrorResponse { eMessage = "Token expired." }
