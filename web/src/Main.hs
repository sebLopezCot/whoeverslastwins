{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Miso
    ( App(App), View, defaultEvents, div_, events, initialAction
    , model, mountPoint, noEff, startApp, subs, text, update, view
    )

main :: IO ()
main = startApp App
    { model = ()
    , update = const noEff
    , view = mainView
    , initialAction = ()
    , events = defaultEvents
    , mountPoint = Nothing
    , subs = []
    }

mainView :: () -> View ()
mainView _ = div_ [] []
