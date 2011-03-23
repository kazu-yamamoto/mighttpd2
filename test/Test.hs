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
          ,("User",CV_String "root")
          ,("Group",CV_String "wheel")
          ,("Pid_File",CV_String "/var/run/mighty.pid")
          ,("Log_File",CV_String "/var/log/mighty")
          ,("Log_File_Size",CV_Int 16777216)
          ,("Log_Backup_Number",CV_Int 10)
          ,("Log_Buffer_Size",CV_Int 16384)
          ,("Log_Flush_Period",CV_Int 10)
          ,("Index_File",CV_String "index.html")]

test_route :: Assertion
test_route = do
    res <- parseRoute "sample.route"
    res @?= ans
 where
    ans = [Block ["localhost","www.example.com"]
           [Mapper "/~alice/cgi-bin/" OpCGI "/home/alice/public_html/cgi-bin/"
           ,Mapper "/~alice/" OpFile "/home/alice/public_html/"
           ,Mapper "/cgi-bin/" OpCGI "/export/cgi-bin/"
           ,Mapper "/" OpFile "/export/www/"]]

----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
