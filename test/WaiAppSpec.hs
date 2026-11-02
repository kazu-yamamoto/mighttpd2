{-# LANGUAGE OverloadedStrings #-}

module WaiAppSpec where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as H
import Network.HTTP.Types (Status, statusCode)
import Network.Wai.Application.Classic hiding ((</>))
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory
import System.FilePath ((</>))
import Test.Hspec

import Program.Mighty
import WaiApp

spec :: Spec
spec = do
    describe "isHiddenPath" $ do
        it "detects segments starting with a dot" $ do
            isHiddenPath "/foo/bar.html" `shouldBe` False
            isHiddenPath "/a.d/b" `shouldBe` False
            isHiddenPath "/.git/config" `shouldBe` True
            isHiddenPath "/foo/.htaccess" `shouldBe` True
            isHiddenPath "/foo/../bar" `shouldBe` True
        it "splits segments by backslash too" $
            isHiddenPath "/foo\\.git\\config" `shouldBe` True
        it "allows /.well-known/ but not hidden segments under it" $ do
            isHiddenPath "/.well-known/acme-challenge/x" `shouldBe` False
            isHiddenPath "/.well-known/.git/x" `shouldBe` True
            isHiddenPath "/foo/.well-known/x" `shouldBe` True

    describe "getBlock" $ do
        it "finds the block of the domain or the wildcard" $ do
            getBlock "localhost" db `shouldBe` Just [fileRoute]
            getBlock "example.org" db `shouldBe` Just [cgiRoute]
            getBlock "example.org" (take 1 db) `shouldBe` Nothing

    describe "getRoute" $ do
        let routes = [cgiRoute, fileRoute]
        it "finds the route whose source is a prefix" $ do
            getRoute "/cgi-bin/foo" routes `shouldBe` Found cgiRoute
            getRoute "/index.html" routes `shouldBe` Found fileRoute
        it "redirects to the mount point" $
            getRoute "/cgi-bin" routes `shouldBe` Redirect
        it "fails if no route matches" $
            getRoute "/foo" [cgiRoute] `shouldBe` Fail

    describe "fileCgiApp" $ around withSite $ do
        it "matches the domain case-insensitively" $ \site -> do
            get site "localhost" "/" `shouldReturn` 200
            get site "LOCALHOST" "/" `shouldReturn` 200
            get site "localhost." "/" `shouldReturn` 200
            get site "LocalHost:8080" "/" `shouldReturn` 200
        it "returns 412 for an unknown domain" $ \site ->
            get site "example.org" "/" `shouldReturn` 412
        it "rejects hidden files even if encoded" $ \site -> do
            get site "localhost" "/.git/config" `shouldReturn` 400
            get site "localhost" "/%2egit/config" `shouldReturn` 400
            get site "localhost" "/%5c.git/config" `shouldReturn` 400
        it "allows /.well-known/" $ \site ->
            get site "localhost" "/.well-known/acme/x" `shouldReturn` 200

----------------------------------------------------------------

fileRoute, cgiRoute :: Route
fileRoute = RouteFile "/" "/var/www/"
cgiRoute = RouteCGI "/cgi-bin/" "/var/cgi-bin/"

db :: RouteDB
db = [Block ["localhost"] [fileRoute], Block ["*"] [cgiRoute]]

-- | The port of 'fileCgiApp' and a manager shared by requests.
type Site = (Int, H.Manager)

-- | Running 'fileCgiApp' on a temporary document root.
withSite :: (Site -> IO ()) -> IO ()
withSite body = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "mighty-waiapp-spec"
    bracket (mkSite root) removeDirectoryRecursive $ \_ -> do
        rdr <- newRouteDBRef [Block ["localhost"] [RouteFile "/" (fromString (root ++ "/"))]]
        mgr <- H.newManager H.defaultManagerSettings
        let cspec = ClassicAppSpec "Test" "/nonexistent/"
            filespec = FileAppSpec "index.html" (".html" `isSuffixOf`)
            cgispec = CgiAppSpec "index.cgi"
            app = fileCgiApp cspec filespec cgispec (RevProxyAppSpec mgr) rdr
        testWithApplication (return app) $ \port -> body (port, mgr)
  where
    mkSite root = do
        removePathForcibly root
        createDirectoryIfMissing True $ root </> ".git"
        createDirectoryIfMissing True $ root </> ".well-known" </> "acme"
        writeFile (root </> "index.html") "top\n"
        writeFile (root </> ".git" </> "config") "secret\n"
        writeFile (root </> ".well-known" </> "acme" </> "x") "token\n"
        return root

-- | Sending GET with the Host header and the raw path.
get :: Site -> ByteString -> ByteString -> IO Int
get (port, mgr) host path = do
    req <- H.parseRequest $ "http://127.0.0.1:" ++ show port
    let req' = req{H.path = path, H.requestHeaders = [("Host", host)]}
    statusCode' . H.responseStatus <$> H.httpLbs req' mgr
  where
    statusCode' :: Status -> Int
    statusCode' = statusCode
