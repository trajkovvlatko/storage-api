module Lib.Error
  ( ErrorResponse (ErrorResponse),
    eMessage,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import GHC.Generics (Generic)

newtype ErrorResponse = ErrorResponse {eMessage :: String} deriving (Generic)

instance ToJSON ErrorResponse where
  toEncoding (ErrorResponse message) =
    pairs $ "message" .= message
