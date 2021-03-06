{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Games (gamesServer) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.DateTime (addMinutes, getCurrentTime)
import Database.Persist (entityKey, entityVal, get, insert_, selectList, updateGet, (=.))
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, err403, err404, (:<|>)((:<|>)))

import Api.Games
import Models.Game
import Utils

gamesServer :: ServerT GamesApi (ReaderT SqlBackend Handler)
gamesServer = createGame :<|> getAllGames :<|> getGame :<|> playGame
  where
    createGame _ gc = do
        time <- liftIO getCurrentTime
        let game = Game
                { gamePlayer1 = createPlayer1 gc
                , gamePlayer2 = createPlayer2 gc
                , gameTurn = 1
                , gameTimeout = addMinutes 1440 time
                }
        insert_ game
        pure game

    getAllGames _ = do
        games <- selectList [] []
        pure $ entityVal <$> games

    getGame _ id_ = do
        gameM <- get id_
        maybe (throwError err404) pure gameM

    playGame ma gId uId = do
        user <- authUser ma
        when (entityKey user /= uId) $ throwError err403
        gameM <- get gId
        game <- maybe (throwError err404) pure gameM
        time <- liftIO getCurrentTime
        case gameTurn game of
            _ | time > gameTimeout game -> pure game
            1 | gamePlayer1 game == uId ->
                updateGet gId [ GameTurn =. 2, GameTimeout =. addMinutes 1440 time ]
            2 | gamePlayer2 game == uId ->
                updateGet gId [ GameTurn =. 1, GameTimeout =. addMinutes 1440 time ]
            _ -> pure game
