{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types
    ( FromJSON, ToJSON, Value(Object), object
    , parseJSON, toJSON, typeMismatch, (.:), (.=)
    )
import Data.Bool (bool)
import Data.Foldable (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
    ( Application, Capture, Delete, Get, JSON, Patch, Post
    , Proxy(Proxy), ReqBody, Server, serve, (:<|>)((:<|>)), (:>)
    )

data Player = Player
    { id_ :: Int
    , username :: String
    , password :: String
    , score :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Player where
    toJSON pl = object
        [ "id" .= id_ pl
        , "username" .= username pl
        , "password" .= password pl
        , "score" .= score pl
        ]

data PlayerCreate = PlayerCreate
    { createUsername :: String
    , createPassword :: String
    } deriving (Eq, Show, Generic)

instance FromJSON PlayerCreate where
    parseJSON (Object v) = PlayerCreate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "PlayerCreate" invalid

data PlayerUpdate = PlayerUpdate
    { updateUsername :: Maybe String
    , updatePassword :: Maybe String
    } deriving (Eq, Show, Generic)

instance FromJSON PlayerUpdate where
    parseJSON (Object v) = PlayerUpdate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "PlayerUpdate" invalid

type API = "users" :> ReqBody '[JSON] PlayerCreate :> Post '[JSON] Player
      :<|> "users" :> Get '[JSON] [Player]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] Player 
      :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] PlayerUpdate :> Patch '[JSON] ()
      :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

server :: MVar [Player] -> Server API
server players = createPlayer
            :<|> getAllPlayers
            :<|> getPlayer
            :<|> updatePlayer
            :<|> deletePlayer
  where
    createPlayer pc = liftIO . modifyMVar players $ \ps -> do
        let pl = Player
                { id_ = succ . maximum . (0 :) $ id_ <$> ps
                , username = createUsername pc
                , password = createPassword pc
                , score = 0
                }
        pure (pl : ps, pl)

    getAllPlayers = liftIO $ readMVar players

    getPlayer id' = liftIO $ do
        ps <- readMVar players
        pure . fromJust $ find ((== id') . id_) ps

    updatePlayer id' pu = liftIO . modifyMVar players $ \ps -> do
        let update pl = flip (bool pl) (id' == id_ pl) $ pl
                { username = fromMaybe (username pl) (updateUsername pu)
                , password = fromMaybe (password pl) (updatePassword pu)
                }
        pure (update <$> ps, ())

    deletePlayer id' = liftIO . modifyMVar players $ \ps ->
        pure (take id' ps <> drop (id' + 1) ps, ())

app :: MVar [Player] -> Application
app = serve @API Proxy . server

main :: IO ()
main = do
    players <- newMVar []
    run 8081 $ app players
