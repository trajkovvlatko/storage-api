module Lib.Auth where
import Web.Scotty (liftAndCatchIO)
import ClassyPrelude (Utf8 (decodeUtf8), encodeUtf8, readFile, ByteString, Text, fromMaybe)
import Database.PostgreSQL.Simple.Newtypes (Aeson(Aeson))
import qualified Data.Aeson as Aeson
import qualified System.Environment as ENV
import Jose.Jwe
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt

type UserId = Text
type Token = ClassyPrelude.Text

data AuthTokenError
  = TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)

getCurrentUserId :: Maybe UserId
getCurrentUserId = Just "asd123"

requireUser :: Either AuthTokenError UserId
requireUser = case getCurrentUserId of
  Nothing -> Left TokenErrorExpired
  _       -> Right "asd"

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
  return "aaa"

generateToken' :: UserId -> IO Token
generateToken' userId = do
  jwks <- getJwks

  encoded <- encode jwks (JwsEncoding RS256) (Claims "public claims")
  print ">>>>>>>>>>>>>>>>>> encoded"
  print encoded
  print ">>>>>>>>>>>>>>>>>>"

  case encoded of
    Left _    -> print "failed encoding"
    Right jwt -> do
      decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (unJwt jwt)
      either (\_ -> print "failed decode")
             (\(Jws (_, bs)) -> print bs)
             decoded

  -- decoded <- decode jwks (JwsEncoding RS256) encoded
  return "some text"

tokenToUserId :: Token -> IO UserId
tokenToUserId token = do
  jwks <- getJwks

  userId <- do
    decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (encodeUtf8 token)
    case decoded of
      Left _   -> return "failed decode"
      Right (Jws (_, bs)) -> return bs
      _ -> return "failed"

  return $ decodeUtf8 userId

userIdToToken :: UserId -> IO Token
userIdToToken userId = do
  jwks <- getJwks

  encoded <- encode jwks (JwsEncoding RS256) (Claims (encodeUtf8 userId))

  case encoded of
    Left je -> return "failed encode"
    Right jwt -> return $ decodeUtf8 $ unJwt jwt

generateToken :: UserId -> IO Token
generateToken userId = do
  jwks <- getJwks

  encoded <- encode jwks (JwsEncoding RS256) (Claims "public claims")

  userId <- do
    case encoded of
      Left _    -> return "failed encoding"
      Right jwt -> do
        decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (unJwt jwt)
        either (\_ -> return "failed decode")
               (\(Jws (_, bs)) -> return bs)
               decoded

  return $ decodeUtf8 userId