{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeFamilies #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.DateTime (addMinutes, getCurrentTime)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Database.Persist (delete, entityVal, get, insert_, selectList, update, (=.))
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy(Proxy), ServerT, serve, (:<|>)((:<|>)))
import Servant.Utils.Enter ((:~>)(NT), enter)

import Api
import Api.Games
import Api.Users
import Models.Game
import Models.User

server :: ServerT Api (ReaderT SqlBackend IO)
server = (createUser :<|> getAllUsers :<|> getUser :<|> updateUser :<|> deleteUser)
    :<|> (createGame :<|> getAllGames :<|> getGame)
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

    createGame gc = do
        time <- liftIO getCurrentTime
        let game = Game
                { gamePlayer1 = createPlayer1 gc
                , gamePlayer2 = createPlayer2 gc
                , gameTurn = 0
                , gameTimeout = addMinutes 1440 time
                }
        insert_ game
        pure game

    getAllGames = do
        games <- selectList [] []
        pure $ entityVal <$> games

    getGame id_ = do
        userM <- get id_
        pure $ fromJust userM

app :: SqlBackend -> Application
app db = serve @Api Proxy $ enter (NT $ liftIO . flip runReaderT db) server

main :: IO ()
main = runNoLoggingT . withSqliteConn "wlw.db" $ \db -> do
    runReaderT (runMigration migrateUser) db
    runReaderT (runMigration migrateGame) db
    liftIO . run 8081 $ app db
