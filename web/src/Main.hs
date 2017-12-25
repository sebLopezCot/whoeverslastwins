{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import React.Flux (View, div_, mkView, reactRenderView)

main :: IO ()
main = reactRenderView "content" baseView

baseView :: View ()
baseView = mkView "" $ \() -> div_ [] ""
