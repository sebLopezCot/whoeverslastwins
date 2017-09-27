{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Database.Persist (delete, entityVal, get, insert_, selectList, update, (=.))
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy(Proxy), ServerT, serve, (:<|>)((:<|>)))
import Servant.Utils.Enter ((:~>)(NT), enter)

import Api
import Models.User

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
        userM <- get id_
        pure $ fromJust userM

    updateUser id_ pu = update id_ . toList $ (UserPassword =.) <$> updatePassword pu

    deleteUser = delete

app :: SqlBackend -> Application
app db = serve @API Proxy $ enter (NT $ liftIO . flip runReaderT db) server

main :: IO ()
main = runNoLoggingT . withSqliteConn "wlw.db" $ \db -> do
    runReaderT (runMigration migrateUser) db
    liftIO . run 8081 $ app db
