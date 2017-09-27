{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Api where

import Servant ((:<|>))

import Api.Users

type API = UsersAPI
