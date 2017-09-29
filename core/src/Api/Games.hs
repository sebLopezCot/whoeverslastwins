{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api.Games (GamesApi, createPlayer1, createPlayer2) where

import Data.Aeson.Types (FromJSON, Value(Object), parseJSON, typeMismatch, (.:))
import Servant
    ( Capture, Delete, Get, JSON, Patch, Post
    , ReqBody, (:<|>), (:>)
    )

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

type GamesApi
       = "games" :> ReqBody '[JSON] GameCreate :> Post '[JSON] Game
    :<|> "games" :> Get '[JSON] [Game]
    :<|> "games" :> Capture "id" GameId :> Get '[JSON] Game
    :<|> "games" :> Capture "gId" GameId :> "play" :> Capture "uId" UserId :> Post '[JSON] Game
