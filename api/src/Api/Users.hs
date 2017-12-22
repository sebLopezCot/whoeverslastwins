{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api.Users (UsersApi, createUsername, createPassword, updatePassword) where

import Data.Aeson.Types (FromJSON, Value(Object), parseJSON, typeMismatch, (.:))
import Servant (Capture, Delete, Get, Header, JSON, Patch, Post, ReqBody, (:<|>), (:>))

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

type UsersUrl a = Header "Authorization" String :> "users" :> a

type UsersApi
       = UsersUrl (ReqBody '[JSON] UserCreate :> Post '[JSON] User)
    :<|> UsersUrl (Get '[JSON] [User])
    :<|> UsersUrl (Capture "id" UserId :> Get '[JSON] User)
    :<|> UsersUrl (Capture "id" UserId :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ())
    :<|> UsersUrl (Capture "id" UserId :> Delete '[JSON] ())
