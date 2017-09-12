{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Models.User
    ( EntityField(UserUsername, UserPassword)
    , User(User, userUsername, userPassword, userScore)
    , UserId
    , migrateUser
    ) where

import Data.Aeson.Types (ToJSON, object, toJSON, (.=))
import Database.Persist (EntityField)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User
    username String
    password String
    score Int
    deriving Show
|]

instance ToJSON User where
    toJSON pl = object
        [ "username" .= userUsername pl
        , "password" .= userPassword pl
        , "score" .= userScore pl
        ]
