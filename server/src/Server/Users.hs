{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Users (usersServer) where

import Control.Monad.Reader (ReaderT)
import Database.Persist
    (Filter(Filter), PersistFilter(Eq), delete, entityVal, insert_, replace, selectList)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, (:<|>)((:<|>)))

import Api.Users
import Models.User
import Utils

usersServer :: ServerT UsersApi (ReaderT SqlBackend Handler)
usersServer = createUser :<|> getAllUsers :<|> getUser :<|> updateUser :<|> deleteUser
  where
    createUser _ pc = do
        let user = User
                { userUsername = createUsername pc
                , userPassword = createPassword pc
                , userScore = 0
                }
        insert_ user
        pure user

    getAllUsers = maybe (pure []) $ \a -> do
        users <- selectList [Filter UserPassword (Left a) Eq] []
        pure $ entityVal <$> users

    getUser = authUser

    updateUser ma id_ pu = do
        user <- authUser ma id_
        flip (maybe $ pure ()) (updatePassword pu) $ \p' ->
            replace id_ $ user { userPassword = p' }

    deleteUser ma id_ = do
        authUser ma id_
        delete id_
