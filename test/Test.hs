{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{-
  % runghc -i.. Test.hs
-}

module Test where

import Config.Internal
import Route
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Types

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_config :: Assertion
case_config = do
    res <- parseConfig "sample.conf"
    res @?= ans
 where
    ans = [("Port",CV_Int 80)
          ,("Debug_Mode",CV_Bool True)
          ,("User",CV_String "nobody")
          ,("Group",CV_String "nobody")
          ,("Pid_File",CV_String "/var/run/mighty.pid")
          ,("Logging",CV_Bool True)
          ,("Log_File",CV_String "/var/log/mighty")
          ,("Log_File_Size",CV_Int 16777216)
          ,("Log_Backup_Number",CV_Int 10)
          ,("Index_File",CV_String "index.html")
          ,("Connection_Timeout",CV_Int 30)
          ,("Worker_Processes",CV_Int 1)]

case_route :: Assertion
case_route = do
    res <- parseRoute "sample.route"
    res @?= ans
 where
    ans = [Block
           ["localhost","www.example.com"]
           [RouteCGI "/~alice/cgi-bin/" "/home/alice/public_html/cgi-bin/"
           ,RouteFile "/~alice/" "/home/alice/public_html/"
           ,RouteCGI "/cgi-bin/" "/export/cgi-bin/"
           ,RouteRevProxy "/app/wiki" "/" "localhost" 8080
           ,RouteRevProxy "/app/cal" "/path" "example.net" 80
           ,RouteFile "/" "/export/www/"]]

----------------------------------------------------------------
