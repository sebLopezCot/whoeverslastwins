{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson.Types (FromJSON, Value(Object), parseJSON, typeMismatch, (.:))
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromJust)
import Database.Persist (delete, entityVal, get, insert_, selectList, update, (=.))
import Database.Persist.Sql (SqlBackend, runMigration, toSqlKey)
import Database.Persist.Sqlite (withSqliteConn)
import Network.Wai.Handler.Warp (run)
import Servant
    ( Application, Capture, Delete, Get, JSON, Patch, Post
    , Proxy(Proxy), ReqBody, ServerT, serve, (:<|>)((:<|>)), (:>)
    )
import Servant.Utils.Enter ((:~>)(NT), enter)

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

data UserUpdate = UserUpdate
    { updateUsername :: Maybe String
    , updatePassword :: Maybe String
    } deriving (Eq, Show)

instance FromJSON UserUpdate where
    parseJSON (Object v) = UserUpdate
        <$> v .: "username"
        <*> v .: "password"
    parseJSON invalid = typeMismatch "UserUpdate" invalid

type API = "users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User
      :<|> "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int64 :> Get '[JSON] User 
      :<|> "users" :> Capture "id" Int64 :> ReqBody '[JSON] UserUpdate :> Patch '[JSON] ()
      :<|> "users" :> Capture "id" UserId :> Delete '[JSON] ()

server :: ServerT API (ReaderT SqlBackend IO)
server = createUser
    :<|> getAllUsers
    :<|> getUser
    :<|> updateUser
    :<|> deleteUser
  where
    createUser pc = do
        let user = User
                { userUsername = createUsername pc
                , userPassword = createPassword pc
                , userScore = 0
                }
        insert_ user
        pure user

    getAllUsers = do
        users <- selectList [] []
        pure $ entityVal <$> users

    getUser id_ = do
        userM <- get $ toSqlKey id_
        pure $ fromJust userM

    updateUser id_ pu =
        update (toSqlKey id_) $ catMaybes
            [ (UserUsername =.) <$> updateUsername pu
            , (UserPassword =.) <$> updatePassword pu
            ]

    deleteUser = delete

app :: SqlBackend -> Application
app db = serve @API Proxy $ enter (NT $ liftIO . flip runReaderT db) server

main :: IO ()
main = runNoLoggingT . withSqliteConn "wlw.db" $ \db -> do
    runReaderT (runMigration migrateUser) db
    liftIO . run 8081 $ app db
