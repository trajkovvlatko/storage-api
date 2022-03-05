module Server
  ( runApp
  , app
  )
where

import qualified Web.Scotty as S
import Router ( routes )
import Configuration.Dotenv (defaultConfig, loadFile)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Web.Scotty (scottyApp)

loadEnv :: IO [(String, String)]
loadEnv = loadFile defaultConfig

app :: IO Application
app = scottyApp routes

runApp :: IO ()
runApp = do
  _ <- loadEnv
  app >>= run 3000
