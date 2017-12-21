{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Users (usersServer) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.Foldable (toList)
import Database.Persist (delete, entityVal, get, insert_, selectList, update, (=.))
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, err404, (:<|>)((:<|>)))

import Api.Users
import Models.User

usersServer :: ServerT UsersApi (ReaderT SqlBackend Handler)
usersServer = createUser :<|> getAllUsers :<|> getUser :<|> updateUser :<|> deleteUser
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
        maybe (throwError err404) pure userM

    updateUser id_ pu = update id_ . toList $ (UserPassword =.) <$> updatePassword pu

    deleteUser = delete
