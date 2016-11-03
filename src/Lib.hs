{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras
import qualified Data.Text as T
import Data.Text.IO
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Random (randomRIO)
type Fortune = T.Text

type API = "fortunes" :> Get '[JSON] [Fortune]
       :<|> "fortune" :> Get '[JSON] Fortune 
       :<|> "js" :> Raw
       :<|> Raw
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

getAllFortunes :: IO [Fortune]
getAllFortunes = do
        fortunesTxtContents <- Data.Text.IO.readFile "fortunes.txt"
        return $ T.splitOn "\n" fortunesTxtContents 

getRandomFortune :: [Fortune] -> IO Fortune
getRandomFortune = pick

server :: Server API
server =  liftIO getAllFortunes 
     :<|> liftIO (getAllFortunes >>= getRandomFortune)
     :<|> serveDirectory "./js"
     :<|> serveDirectory "./html"
