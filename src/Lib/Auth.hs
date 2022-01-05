{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib.Auth
  ( tokenToUserId
  , userIdToToken
  , AuthError
  , Token
  , UserId
  ) where

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

data AuthError
  = TokenDecodeError
  | TokenEncodeError
  deriving (Eq, Show)

getJwks :: IO [Jwk]
getJwks = do
  jwkSig <- ClassyPrelude.readFile ".jwk.sig"
  let parsedJwkSig = Aeson.eitherDecodeStrict jwkSig
  return $ either (\e -> error "Error: Cannot parse .jwk.sig.") pure parsedJwkSig

tokenToUserId :: Token -> IO (Either AuthError UserId)
tokenToUserId token = do
  jwks <- getJwks

  decoded <- Jose.Jwt.decode jwks (Just (JwsEncoding RS256)) (encodeUtf8 token)
  case decoded of
    Left _              -> return $ Left TokenDecodeError
    Right (Jws (_, bs)) -> do
      let userId = (readInt . decodeUtf8) bs
      return $ Right userId
    _ -> return $ Left TokenDecodeError

userIdToToken :: UserId -> IO (Either AuthError Token)
userIdToToken userId = do
  jwks <- getJwks

  let bsUserId = (pack . encodeUtf8) $ show userId
  encoded <- Jose.Jwt.encode jwks (JwsEncoding RS256) (Claims bsUserId)

  case encoded of
    Left je -> return $ Left TokenEncodeError
    Right jwt -> return $ Right (getValueFromJwt jwt)

getValueFromJwt :: Jwt -> Text
getValueFromJwt = decodeUtf8 . unJwt

getIntFromText :: Reader a -> Text -> a
getIntFromText = (value .)
  where value (Right (v,_)) = v

readInt :: Text -> Integer
readInt = getIntFromText decimal