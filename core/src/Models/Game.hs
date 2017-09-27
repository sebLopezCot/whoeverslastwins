{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Models.Game
    ( EntityField(GamePlayer1, GamePlayer2, GameTurn, GameTimeout)
    , Game(Game, gamePlayer1, gamePlayer2, gameTurn, gameTimeout)
    , GameId
    , migrateGame
    ) where

import Data.Aeson.Types (ToJSON, object, toJSON, (.=))
import Data.DateTime (DateTime)
import Database.Persist (EntityField)
import Database.Persist.TH
    ( derivePersistField, mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )

import Models.User

share [mkPersist sqlSettings, mkMigrate "migrateGame"] [persistLowerCase|
Game
    player1 UserId
    player2 UserId
    turn Int
    timeout DateTime
    deriving Show
|]

instance ToJSON Game where
    toJSON pl = object
        [ "player1" .= gamePlayer1 pl
        , "player2" .= gamePlayer2 pl
        , "turn" .= gameTurn pl
        , "timeout" .= gameTimeout pl
        ]
