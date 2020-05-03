{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Program.Mighty.Dhall.Option where

import Dhall.TH

Dhall.TH.makeHaskellTypes
    [ SingleConstructor "Option" "MakeOption" "./Program/Mighty/Dhall/Option.dhall"
    ]