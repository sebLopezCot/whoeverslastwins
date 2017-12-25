module Utils (authUser) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Database.Persist
    (Entity, Filter(Filter), PersistFilter(Eq), entityVal, getEntity, selectFirst)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler, err401)

import Models.User
import Models.UserToken

authUser :: Maybe String -> ReaderT SqlBackend Handler (Entity User)
authUser = maybe (throwError err401) $ \a -> do
    tokenM <- selectFirst [Filter UserTokenToken (Left a) Eq] []
    flip (maybe $ throwError err401) tokenM $ \token -> do
        userM <- getEntity . userTokenUser $ entityVal token
        maybe (throwError err401) pure userM
