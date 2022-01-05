module Lib.Error 
  ( ErrorResponse (ErrorResponse)
  , eMessage
  ) where

import Data.Aeson (ToJSON(toJSON, toEncoding), KeyValue((.=)), pairs)
import GHC.Generics (Generic)

newtype ErrorResponse = ErrorResponse { eMessage :: String } deriving Generic

instance ToJSON ErrorResponse where
  toEncoding (ErrorResponse eMessage) =
    pairs $ "message" .= eMessage