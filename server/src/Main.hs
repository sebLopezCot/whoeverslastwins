{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy(Proxy), serve)
import Servant.Utils.Enter ((:~>)(Nat), enter)

import Api
import Models.Game
import Models.User
import Models.UserToken

import Server

app :: SqlBackend -> Application
app db = serve @Api Proxy $ enter (Nat $ flip (runReaderT @_ @_ @Handler) db) server

main :: IO ()
main = runNoLoggingT . withSqliteConn "wlw.db" $ \db -> do
    flip runReaderT db $ do
        runMigration migrateUser
        runMigration migrateUserToken
        runMigration migrateGame
    liftIO . run 8081 $ app db
