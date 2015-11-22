{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan,
                                               readTChan, writeTChan)
import           Control.Exception            (bracket)
import           Control.Monad                (forever)
import           Control.Monad.STM            (atomically)
import           Data.Text                    (isInfixOf, pack)
import           System.Exit                  (exitFailure)
import           System.FilePath.Posix        (takeBaseName)
import           System.FSNotify              (Event (..), watchTree,
                                               withManager)
import           System.IO                    (BufferMode (..), Handle,
                                               hPutStrLn, hSetBuffering, stderr)
import           System.Process

main :: IO ()
main = do
    chan <- atomically newTChan
    _ <- forkIO $ watchThread chan
    _ <- replThread chan
    return ()

watchThread :: TChan Event -> IO ()
watchThread writeChan = withManager $ \mgr -> do
    -- start a watching job (in the background)
    _ <- watchTree mgr "." shouldReload (reloadApplication writeChan)
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000000

replThread :: TChan Event -> IO ()
replThread chan = do
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
            print event
            putStrLn "-----------------------------"
            hPutStrLn replIn loadString
            hPutStrLn replIn startString
    onSuccess _ (_, _, _, _) = do
        hPutStrLn stderr "Can't open GHCi's stdin"
        exitFailure

    startString = "update"
    loadString = ":load app/DevelMain.hs"

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
                 , notInFile "stack.yaml"
                 , notInFile "devel-main-since"
                 ]
    notInPath t = t `isInfixOf` pack fp
    notInFile t = t `isInfixOf` pack (takeBaseName fp)

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
