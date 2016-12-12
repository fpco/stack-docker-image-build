{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Process.Typed
import System.FilePath
import Data.Foldable
import System.Directory
import System.Environment (getArgs)
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Yaml
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Data.Maybe

parseStackYaml :: IO ([String], [String])
parseStackYaml = do
    val <- decodeFileEither "stack.yaml" >>= either throwIO return
    let buildFirst = fromMaybe [] $ do
            Object o1 <- Just val
            Object o2 <- HashMap.lookup "x-stack-docker-image-build" o1
            Array vals <- HashMap.lookup "build-first" o2
            Just [T.unpack dep | String dep <- V.toList vals]
        extraDeps = fromMaybe [] $ do
            Object o <- Just val
            Array vals <- HashMap.lookup "extra-deps" o
            Just [T.unpack dep | String dep <- V.toList vals]
    return (buildFirst, extraDeps)

stack :: [String] -> ProcessConfig () () ()
stack args = proc "stack" $ ["--no-install-ghc", "--system-ghc"] ++ args

runStack :: [String] -> IO ()
runStack = runProcess_ . stack

readStack :: [String] -> IO String
readStack = fmap (TL.unpack . decodeUtf8 . fst) . readProcess_ . stack

getDir :: String -> IO FilePath
getDir flag = do
    dirRaw <- readStack ["path", flag]
    return $ takeWhile (/= '\n') dirRaw

getDBDir :: String -> IO FilePath
getDBDir typ = getDir $ concat ["--", typ, "-pkg-db"]

getBinDir :: String -> IO FilePath
getBinDir typ = do
    dir <- getDir $ concat ["--", typ, "-install-root"]
    return $ dir </> "bin"

main :: IO ()
main = do
    args <- getArgs
    (buildFirst, deps) <- parseStackYaml

    forM_ buildFirst $ \pkg -> do
        putStrLn $ "Building " ++ pkg ++ " from build-first"
        runStack $ "build" : words pkg ++ args

    unless (null deps) $ do
        putStrLn "Building extra-deps"
        runStack $ "build" : deps ++ args

    putStrLn "Performing build local"
    runStack $ "build" : args

    globaldb <- getDBDir "global"
    forM_ (words "snapshot local") $ \typ -> do
        bindir <- getBinDir typ
        bindirexists <- doesDirectoryExist bindir
        bincontents <- if bindirexists then getDirectoryContents bindir else return []
        forM_ bincontents $ \file -> do
            let fp = bindir </> file
            exists <- doesFileExist fp
            when exists $ do
                putStrLn $ "Linking " ++ fp
                runProcess_ $ proc "ln" [fp, "/usr/local/bin" </> file]

        dbdir <- getDBDir typ
        dbdirexists <- doesDirectoryExist dbdir
        dbcontents <- if dbdirexists then getDirectoryContents dbdir else return []
        forM_ dbcontents $ \file -> when (takeExtension file == ".conf") $ do
            let fp = dbdir </> file
            putStrLn $ "Registering: " ++ file
            runStack
                [ "exec"
                , "--"
                , "ghc-pkg"
                , "register"
                , fp
                , "--package-db"
                , globaldb
                , "--force"
                ]

    stackDir <- getAppUserDataDirectory "stack"
    stackContents <- getDirectoryContents stackDir
    let toKeep "." = True
        toKeep ".." = True
        toKeep "snapshots" = True
        toKeep _ = False
    forM_ (filter (not . toKeep) stackContents) $ \x ->
        runProcess_ $ proc "rm" ["-rf", stackDir </> x]
