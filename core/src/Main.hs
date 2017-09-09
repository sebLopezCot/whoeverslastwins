{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Aeson.Types
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

data Player = Player
    { id :: Int
    , username :: String
    , password :: String
    , score :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Player

data PlayerCreate = PlayerCreate
    { inputUsername :: String
    , inputPassword :: String
    } deriving (Eq, Show, Generic)

instance FromJSON PlayerCreate

type API = "users" :> ReqBody '[JSON] PlayerCreate :> Post '[JSON] Player
      :<|> "users" :> Get '[JSON] [Player]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] Player 
      :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] PlayerCreate :> Patch '[JSON] ()
      :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

server :: Server API
server = createPlayer
    :<|> getAllPlayers
    :<|> getPlayer
    :<|> updatePlayer
    :<|> deletePlayer
  where
    createPlayer info = pure (Player 0 "Seb" "p@ssword" 350)

    getAllPlayers = pure [Player 0 "Seb" "p@ssword" 350]
    
    getPlayer id_ = pure (Player 0 "Seb" "p@ssword" 350)

    updatePlayer id_ info = pure ()

    deletePlayer id_ = pure ()

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app


