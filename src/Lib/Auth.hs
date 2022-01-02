{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib.Auth where
import Web.Scotty (liftAndCatchIO)
import ClassyPrelude (Utf8 (decodeUtf8), encodeUtf8, readFile, ByteString, Text, pack)
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Data.Aeson as Aeson
import qualified System.Environment as ENV
import Jose.Jwe
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt
import Data.Text.Read (decimal, Reader)

type UserId = Integer
type Token = ClassyPrelude.Text

data AuthTokenError
  = TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)

getJwks :: IO [Jwk]
getJwks = do
  jwkSig <- ClassyPrelude.readFile ".jwk.sig"
  let parsedJwkSig = Aeson.eitherDecodeStrict jwkSig
  return $ either (\e -> error "Error: Cannot parse .jwk.sig.") pure parsedJwkSig

tokenToUserId :: Token -> IO UserId
tokenToUserId token = do
  jwks <- getJwks

  do
    decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (encodeUtf8 token)
    case decoded of
      Left _   -> return 0
      Right (Jws (_, bs)) -> do
        let str = decodeUtf8 bs
        return $ readInt str
      _ -> return 0

userIdToToken :: UserId -> IO Token
userIdToToken userId = do
  jwks <- getJwks

  let bsUserId = (pack . encodeUtf8) $ show userId
  encoded <- Jose.Jwt.encode jwks (JwsEncoding RS256) (Claims bsUserId)

  case encoded of
    Left je -> return "failed encode"
    Right jwt -> return $ decodeUtf8 $ unJwt jwt

it'sSafeIPromise :: Reader a -> Text -> a
it'sSafeIPromise = (value .)
  where
    value (Right (v,_)) = v

readInt :: Text -> Integer
readInt = it'sSafeIPromise decimal