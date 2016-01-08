{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan,
                                               readTChan, writeTChan)
import           Control.Exception            (bracket)
import           Control.Monad                (forever, unless, when)
import           Control.Monad.STM            (atomically)
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Digest.Pure.MD5         (md5)
import           Data.Maybe                   (isJust)
import           Data.Text                    (isInfixOf, pack)
import           Paths_yesod_fast_devel
import           System.Console.ANSI
import           System.Directory             (copyFile, doesDirectoryExist,
                                               findExecutable)
import           System.Environment           (getArgs)
import           System.Exit
import           System.FilePath              (takeDirectory)
import           System.FilePath.Glob
import           System.FilePath.Posix        (takeBaseName)
import           System.FSNotify              (Event (..), watchTree,
                                               withManager)
import           System.IO                    (BufferMode (..), Handle,
                                               hPutStrLn, hSetBuffering, stderr,
                                               stdout)
import           System.Process

main :: IO ()
main = getArgs >>= \case
    ("init":develMainPth:_) -> initYesodFastDevel develMainPth
    ("init":_) -> initYesodFastDevel "app/DevelMain.hs"
    ("print-patched-main":_) ->
        putStrLn =<< readFile =<< getDataFileName "PatchedDevelMain.hs"
    (develMainPth:_) -> go develMainPth
    [] -> go "app/DevelMain.hs"
  where
    go develMainPth = do
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
        chan <- atomically newTChan
        _ <- forkIO $ watchThread chan
        _ <- forkIO browserSyncThread
        _ <- replThread develMainPth chan
        return ()

initYesodFastDevel :: FilePath -> IO ()
initYesodFastDevel develMainPth = do
    verifyDirectory
    verifyDevelMain
    patchedDevelMain <- getDataFileName "PatchedDevelMain.hs"
    copyFile patchedDevelMain develMainPth
    putStrLn "Patched `DevelMain.hs`"
    browserSyncPth <- findExecutable "browser-sync"
    putStrLn "Make sure you have `foreign-store` on your cabal file"
    when (not (isJust browserSyncPth)) $ do
        putStrLn "Install `browser-sync` to have livereload at port 4000"
    exitSuccess
  where
    verifyDirectory = do
        let dir = takeDirectory develMainPth
        putStrLn ("Verifying `" ++ dir ++ "` exists")
        dexists <- doesDirectoryExist dir
        unless dexists $ do
            hPutStrLn stderr ("Directory `" ++ dir ++ "` not found")
            exitFailure
    verifyDevelMain = do
        putStrLn "Verifying `DevelMain.hs` isn't modified"
        userDevelMd5 <- md5 <$> ByteString.readFile develMainPth
        originalDevelMd5 <- md5 <$>
            (ByteString.readFile =<< getDataFileName "OriginalDevelMain.hs")
        patchedDevelMd5 <- md5 <$>
            (ByteString.readFile =<< getDataFileName "PatchedDevelMain.hs")

        when (userDevelMd5 == patchedDevelMd5) $ do
            putStrLn "DevelMain.hs is already patched"
            exitSuccess
        when (userDevelMd5 /= originalDevelMd5) $ do
            hPutStrLn stderr "Found a weird DevelMain.hs on your project"
            hPutStrLn stderr "Use `yesod-fast-devel print-patched-main`"
            exitFailure

browserSyncThread :: IO ()
browserSyncThread = do
    browserSyncPth <- findExecutable "browser-sync"
    if (isJust browserSyncPth)
       then callCommand cmd
       else return ()
  where
    cmd = "browser-sync start --no-open --files=\"devel-main-since\" --proxy \"localhost:3000\" --port 4000"

watchThread :: TChan Event -> IO ()
watchThread writeChan = withManager $ \mgr -> do
    -- start a watching job (in the background)
    _ <- watchTree mgr "." shouldReload (reloadApplication writeChan)
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000000

replThread :: FilePath -> TChan Event -> IO ()
replThread develMainPth chan = do
    readChan <- atomically (dupTChan chan)
    bracket newRepl onError (onSuccess readChan)
  where
    onError (_, _, _, process) = do
        interruptProcessGroupOf process
        threadDelay 100000
        terminateProcess process
        threadDelay 100000
        waitForProcess process

    onSuccess readChan (Just replIn, _, _, _) = do
        hSetBuffering replIn LineBuffering
        threadDelay 1000000
        hPutStrLn replIn loadString
        hPutStrLn replIn startString
        forever $ do
            event <- atomically (readTChan readChan)
            putStrLn "-----------------------------"
            setSGR [ SetColor Foreground Vivid Yellow ]
            print event
            setSGR [ Reset ]
            putStrLn "-----------------------------"
            hPutStrLn replIn loadString
            hPutStrLn replIn startString
    onSuccess _ (_, _, _, _) = do
        hPutStrLn stderr "Can't open GHCi's stdin"
        exitFailure

    startString = "update"
    loadString = ":load " ++ develMainPth

shouldReload :: Event -> Bool
shouldReload event = not (or conditions)
  where
    fp = case event of
        Added filePath _ -> filePath
        Modified filePath _ -> filePath
        Removed filePath _ -> filePath
    conditions = [ notInPath ".git"
                 , notInPath "yesod-devel"
                 , notInPath "dist"
                 , notInFile "#"
                 , notInPath ".cabal-sandbox"
                 , notInFile "flycheck_"
                 , notInPath ".stack-work"
                 , notInGlob (compile "**/*.sqlite3-*")
                 , notInGlob (compile "*.sqlite3-*")
                 , notInFile "stack.yaml"
                 , notInGlob (compile "*.hi")
                 , notInGlob (compile "**/*.hi")
                 , notInGlob (compile "*.o")
                 , notInGlob (compile "**/*.o")
                 , notInFile "devel-main-since"
                 ]
    notInPath t = t `isInfixOf` pack fp
    notInFile t = t `isInfixOf` pack (takeBaseName fp)
    notInGlob pt = match pt fp

reloadApplication :: TChan Event -> Event -> IO ()
reloadApplication chan event = atomically (writeTChan chan event)

newRepl :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
newRepl = createProcess $ newProc "stack" [ "ghci"
                                          , "--ghc-options"
                                          , "-O0 -fobject-code"
                                          ]

newProc :: FilePath -> [String] -> CreateProcess
newProc cmd args = CreateProcess { cmdspec = RawCommand cmd args
                                 , cwd = Nothing
                                 , env = Nothing
                                 , std_in = CreatePipe
                                 , std_out = Inherit
                                 , std_err = Inherit
                                 , close_fds = False
                                 , create_group = True
                                 , delegate_ctlc = False
                                 }
