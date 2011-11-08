{-# LANGUAGE OverloadedStrings #-}

{-
  % runghc -i.. Test.hs
-}

module Test where

import Config.Internal
import Route
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Types
tests :: [Test]
tests = [
    testGroup "Parser" [
         testCase "config" test_config
       , testCase "route" test_route
       ]
  ]

----------------------------------------------------------------

test_config :: Assertion
test_config = do
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

test_route :: Assertion
test_route = do
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

main :: Assertion
main = defaultMain tests
