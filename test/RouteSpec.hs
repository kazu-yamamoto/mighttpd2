{-# LANGUAGE OverloadedStrings #-}

module RouteSpec where

import Test.Hspec

import Program.Mighty

spec :: Spec
spec = do
    describe "parseRoute" $ do
        it "parses example.route correctly" $ do
            res <- parseRoute "conf/example.route" "localhost" 80
            res `shouldBe` ans

ans :: [Block]
ans = [Block ["localhost","www.example.com"] [RouteCGI "/~alice/cgi-bin/" "/home/alice/public_html/cgi-bin/",RouteFile "/~alice/" "/home/alice/public_html/",RouteCGI "/cgi-bin/" "/export/cgi-bin/",RouteRevProxy "/app/cal/" "/calendar/" "example.net" 80,RouteRevProxy "/app/wiki/" "/" "127.0.0.1" 3000,RouteFile "/" "/export/www/"]]
