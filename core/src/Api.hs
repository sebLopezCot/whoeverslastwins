{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api where

import Data.Aeson.Types (FromJSON, Value(Object), parseJSON, typeMismatch, (.:))
import Servant
    ( Capture, Delete, Get, JSON, Patch, Post
    , ReqBody, (:<|>), (:>)
    )

import Models.User

data UserCreate = UserCreate
    { createUsername :: String
    , createPassword :: String
    } deriving (Eq, Show)

instance FromJSON UserCreate where
    parseJSON (Object v) = UserCreate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "UserCreate" invalid

newtype UserUpdate = UserUpdate
    { updatePassword :: Maybe String
    } deriving (Eq, Show)

instance FromJSON UserUpdate where
    parseJSON (Object v) = UserUpdate <$> v .: "password"
    parseJSON invalid = typeMismatch "UserUpdate" invalid

type API = "users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User
      :<|> "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" UserId :> Get '[JSON] User 
      :<|> "users" :> Capture "id" UserId :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ()
      :<|> "users" :> Capture "id" UserId :> Delete '[JSON] ()
