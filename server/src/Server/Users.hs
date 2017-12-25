{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Server.Users (usersServer) where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAscii, isAlpha)
import Database.Persist
    ( Filter(Filter), PersistFilter(Eq), delete, entityKey
    , entityVal, insert_, replace, selectFirst
    )
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, ServerT, err400, err401, err403, (:<|>)((:<|>)))
import System.Entropy (getEntropy)

import Api.Users
import Models.User
import Models.UserToken
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

    getAllUsers ma = do 
        user <- authUser ma
        pure [entityVal user]

    getUser ma id_ = do
        user <- authUser ma
        if entityKey user == id_
            then pure $ entityVal user
            else throwError err403

    updateUser ma id_ pu = do
        user <- authUser ma
        when (entityKey user /= id_) $ throwError err403
        flip (maybe $ pure ()) (updatePassword pu) $ \p' ->
            replace id_ $ (entityVal user) { userPassword = p' }

    deleteUser ma id_ = do
        user <- authUser ma
        when (entityKey user /= id_) $ throwError err403
        delete id_

    login ul = do
        userM <- selectFirst [Filter UserUsername (Left $ loginUsername ul) Eq] []
        flip (maybe $ throwError err401) userM $ \user ->
            if validatePassword (pack . userPassword $ entityVal user) (pack $ loginPassword ul)
                then do
                    token <- liftIO $ B.filter (liftA2 (&&) isAscii isAlpha) <$> getEntropy 200
                    let userToken = UserToken
                            { userTokenUser = entityKey user
                            , userTokenToken = unpack token
                            }
                    insert_ userToken
                    pure $ userTokenToken userToken
                else throwError err401
