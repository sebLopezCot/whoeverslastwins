{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api.Users
    ( UsersApi, createUsername, createPassword, loginPassword, loginUsername, updatePassword
    ) where

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

data UserLogin = UserLogin
    { loginUsername :: String
    , loginPassword :: String
    } deriving (Eq, Show)

instance FromJSON UserLogin where
    parseJSON (Object v) = UserLogin <$> v .: "username" <*> v .: "password"
    parseJSON invalid = typeMismatch "UserLogin" invalid

type Auth a = Header "Authorization" String :> a

type UsersApi
       = Auth ("users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User)
    :<|> Auth ("users" :> Get '[JSON] [User])
    :<|> Auth ("users" :> Capture "id" UserId :> Get '[JSON] User)
    :<|> Auth ("users" :> Capture "id" UserId :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ())
    :<|> Auth ("users" :> Capture "id" UserId :> Delete '[JSON] ())
    :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] String
