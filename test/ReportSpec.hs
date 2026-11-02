{-# LANGUAGE OverloadedStrings #-}

module ReportSpec where

import Control.Exception (bracket)
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Test.Hspec

import Program.Mighty

spec :: Spec
spec = describe "report" $ around withDir $ do
    it "appends to the report file" $ \dir -> do
        let file = dir </> "report"
            rpt = initReporter False file
        report rpt "foo"
        report rpt "bar"
        ls <- lines <$> readFile file
        length ls `shouldBe` 2
    it "does not follow a symbolic link" $ \dir -> do
        let victim = dir </> "victim"
            link = dir </> "link"
        writeFile victim "original\n"
        createSymbolicLink victim link
        report (initReporter False link) "foo"
        readFile victim `shouldReturn` "original\n"

withDir :: (FilePath -> IO ()) -> IO ()
withDir body = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "mighty-report-spec"
    bracket (mk dir) removeDirectoryRecursive body
  where
    mk dir = do
        removePathForcibly dir
        createDirectory dir
        return dir
