{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Users (usersServer) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 (pack, unpack)
import Database.Persist
    ( Filter(Filter), PersistFilter(Eq), delete, entityVal
    , insert_, replace, selectFirst, selectList
    )
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, err400, err401, (:<|>)((:<|>)))

import Api.Users
import Models.User
import Utils

usersServer :: ServerT UsersApi (ReaderT SqlBackend Handler)
usersServer = createUser :<|> getAllUsers :<|> getUser :<|> updateUser :<|> deleteUser :<|> login
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

    login ul = do
        userM <- selectFirst [Filter UserUsername (Left $ loginUsername ul) Eq] []
        flip (maybe $ throwError err401) (entityVal <$> userM) $ \user ->
            if validatePassword (pack $ userPassword user) (pack $ loginPassword ul)
                then pure $ loginPassword ul
                else throwError err401
