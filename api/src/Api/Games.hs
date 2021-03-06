{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api.Games (GamesApi, createPlayer1, createPlayer2) where

import Data.Aeson.Types (FromJSON, Value(Object), parseJSON, typeMismatch, (.:))
import Servant (Capture, Get, Header, JSON, Post, ReqBody, (:<|>), (:>))

import Models.Game
import Models.User

data GameCreate = GameCreate
    { createPlayer1 :: UserId
    , createPlayer2 :: UserId
    } deriving (Eq, Show)

instance FromJSON GameCreate where
    parseJSON (Object v) = GameCreate
        <$> v .: "player1"
        <*> v .: "player2"
    parseJSON invalid = typeMismatch "GameCreate" invalid

type GamesUrl a = Header "Authorization" String :> "games" :> a

type GamesApi
       = GamesUrl (ReqBody '[JSON] GameCreate :> Post '[JSON] Game)
    :<|> GamesUrl (Get '[JSON] [Game])
    :<|> GamesUrl (Capture "id" GameId :> Get '[JSON] Game)
    :<|> GamesUrl (Capture "gId" GameId :> "play" :> Capture "uId" UserId :> Post '[JSON] Game)
