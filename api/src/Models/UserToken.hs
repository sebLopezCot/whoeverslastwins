{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Models.UserToken
    ( EntityField(UserTokenUser, UserTokenToken)
    , UserToken(UserToken, userTokenUser, userTokenToken)
    , UserTokenId
    , migrateUserToken
    ) where

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Models.User

share [mkPersist sqlSettings, mkMigrate "migrateUserToken"] [persistLowerCase|
UserToken
    user UserId
    token String
    deriving Show
|]
