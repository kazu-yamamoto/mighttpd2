{-# LANGUAGE OverloadedStrings #-}

{-
  % runghc -i.. Test.hs
-}

module Main where

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
    ans = [("Port",CV_Int 8080),("Debug_Mode",CV_Bool True),("User",CV_String "nobody"),("Group",CV_String "nobody"),("Pid_File",CV_String "/var/run/mighty.pid"),("Index_File",CV_String "index.html")]

test_route :: Assertion
test_route = do
    res <- parseRoute "sample.route"
    res @?= ans
 where
    ans = [Block ["localhost"] [Mapper "/~kazu/cgi-bin/" OpCGI "/Users/kazu/Sites/cgi-bin/",Mapper "/~kazu/" OpFile "/Users/kazu/Sites/",Mapper "/runhaskell/cgi-bin/" OpCGI "/Users/kazu/work/runhaskell/cgi-bin/",Mapper "/runhaskell/" OpFile "/Users/kazu/work/runhaskell/",Mapper "/monad/" OpFile "/Users/kazu/work/monad/html/",Mapper "/" OpFile "/Users/kazu/Mew.org/"]]

----------------------------------------------------------------

main :: Assertion
main = defaultMain tests
