{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Users (usersServer) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 (pack, unpack)
import Database.Persist (delete, entityVal, insert_, replace, selectList)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, err400, (:<|>)((:<|>)))

import Api.Users
import Models.User
import Utils

usersServer :: ServerT UsersApi (ReaderT SqlBackend Handler)
usersServer = createUser :<|> getAllUsers :<|> getUser :<|> updateUser :<|> deleteUser
  where
    createUser _ pc = do
        let pwd = pack $ createPassword pc
        mhPwd <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy pwd
        hPwd <- maybe (throwError err400) pure mhPwd
        let user = User
                { userUsername = createUsername pc
                , userPassword = unpack hPwd
                , userScore = 0
                }
        insert_ user
        pure user

    getAllUsers = maybe (pure []) $ \a -> do
        users <- selectList [] []
        pure . filter (flip validatePassword (pack a) . pack . userPassword) $ entityVal <$> users

    getUser = authUser

    updateUser ma id_ pu = do
        user <- authUser ma id_
        flip (maybe $ pure ()) (updatePassword pu) $ \p' ->
            replace id_ $ user { userPassword = p' }

    deleteUser ma id_ = do
        authUser ma id_
        delete id_
