{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Program.Mighty.Dhall.Route where

import Dhall.TH

Dhall.TH.makeHaskellTypes
    [ MultipleConstructors "Route" "./Program/Mighty/Dhall/Route.dhall"
    , SingleConstructor "Block" "MakeBlock" "./Program/Mighty/Dhall/Block.dhall"
    , SingleConstructor "RouteDB" "MakeRouteDB" "./Program/Mighty/Dhall/RouteDB.dhall"
    ]