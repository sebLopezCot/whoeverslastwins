module Utils (authUser) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Database.Persist (get)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, err401, err403, err404)

import Models.User

authUser :: Maybe String -> UserId -> ReaderT SqlBackend Handler User
authUser = maybe (\_ -> throwError err401) $ \a id_ -> do
    userM <- get id_
    flip (maybe $ throwError err404) userM $ \user ->
        if userPassword user == a
            then pure user
            else throwError err403
