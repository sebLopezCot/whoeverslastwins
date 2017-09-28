{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api where

import Servant ((:<|>))

import Api.Games
import Api.Users

type Api = UsersApi :<|> GamesApi
