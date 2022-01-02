module Lib.Auth where
import Web.Scotty (liftAndCatchIO)
import Jose.Jwt (decode, JwtEncoding (JwsEncoding), JwtContent (Jws))
import Jose.Jwe (jwkDecode)
import Jose.Jwa (JwsAlg(RS256))
import ClassyPrelude (Utf8, encodeUtf8, readFile, ByteString, Text)
import Jose.Jwk (Jwk)
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Data.Aeson as Aeson
import qualified System.Environment as ENV
import qualified Jose.Jwt as Jwk

type UserId = Integer
type Token = Text

data AuthTokenError
  = TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)

getCurrentUserId :: Maybe UserId
getCurrentUserId = Just 5

requireUser :: Either AuthTokenError UserId
requireUser = case getCurrentUserId of
  Nothing -> Left TokenErrorExpired
  _       -> Right 123

getJwks :: IO [Jwk]
getJwks = do
  jwkSig <- ClassyPrelude.readFile ".jwk.sig"
  let parsedJwkSig = Aeson.eitherDecodeStrict jwkSig
  return $ either (\e -> error "Error: Cannot parse .jwk.sig.") pure parsedJwkSig

decryptToken :: Token -> IO UserId
decryptToken token = do
  jwks <- getJwks
  eitherJwt <- decode jwks (Just $ JwsEncoding RS256) (encodeUtf8 token)
  either (\_ -> print "Decode Failure") (\(Jws (_, bs)) -> print bs) eitherJwt
  return 1