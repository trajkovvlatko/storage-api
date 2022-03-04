module Server
  ( start
  )
where

import qualified Web.Scotty as S
import Router ( routes )
import Configuration.Dotenv (defaultConfig, loadFile)

loadEnv :: IO [(String, String)]
loadEnv = loadFile defaultConfig

start :: IO ()
start = do
  _ <- loadEnv
  S.scotty 3000 routes
