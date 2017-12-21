module Server (server) where

import Control.Monad.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, (:<|>)((:<|>)))

import Api

import Server.Games
import Server.Users

server :: ServerT Api (ReaderT SqlBackend Handler)
server = usersServer :<|> gamesServer
