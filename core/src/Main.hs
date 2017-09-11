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

data User = User
    { id_ :: Int
    , username :: String
    , password :: String
    , score :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON User where
    toJSON pl = object
        [ "id" .= id_ pl
        , "username" .= username pl
        , "password" .= password pl
        , "score" .= score pl
        ]

data UserCreate = UserCreate
    { createUsername :: String
    , createPassword :: String
    } deriving (Eq, Show, Generic)

instance FromJSON UserCreate where
    parseJSON (Object v) = UserCreate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "UserCreate" invalid

data UserUpdate = UserUpdate
    { updateUsername :: Maybe String
    , updatePassword :: Maybe String
    } deriving (Eq, Show, Generic)

instance FromJSON UserUpdate where
    parseJSON (Object v) = UserUpdate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "UserUpdate" invalid

type API = "users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User
      :<|> "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User 
      :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ()
      :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

server :: MVar [User] -> Server API
server players = createUser
            :<|> getAllUsers
            :<|> getUser
            :<|> updateUser
            :<|> deleteUser
  where
    createUser pc = liftIO . modifyMVar players $ \ps -> do
        let pl = User
                { id_ = succ . maximum . (0 :) $ id_ <$> ps
                , username = createUsername pc
                , password = createPassword pc
                , score = 0
                }
        pure (pl : ps, pl)

    getAllUsers = liftIO $ readMVar players

    getUser id' = liftIO $ do
        ps <- readMVar players
        pure . fromJust $ find ((== id') . id_) ps

    updateUser id' pu = liftIO . modifyMVar players $ \ps -> do
        let update pl = flip (bool pl) (id' == id_ pl) $ pl
                { username = fromMaybe (username pl) (updateUsername pu)
                , password = fromMaybe (password pl) (updatePassword pu)
                }
        pure (update <$> ps, ())

    deleteUser id' = liftIO . modifyMVar players $ \ps ->
        pure (take id' ps <> drop (id' + 1) ps, ())

app :: MVar [User] -> Application
app = serve @API Proxy . server

main :: IO ()
main = do
    players <- newMVar []
    run 8081 $ app players
