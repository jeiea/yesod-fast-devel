{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan,
                                               readTChan, writeTChan)
import           Control.Exception            (bracket)
import           Control.Monad                (forever, void)
import           Control.Monad.STM            (atomically)
import           Data.Text                    (isInfixOf, pack)
import           System.FilePath.Posix        (takeBaseName)
import           System.FSNotify              (Event (..), watchTree,
                                               withManager)
import           System.IO                    (BufferMode (..), Handle,
                                               hGetLine, hPutStrLn,
                                               hSetBuffering)
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
    onError (_,_,_,process) = do
        interruptProcessGroupOf process
        threadDelay 100000
        terminateProcess process
        threadDelay 100000
        waitForProcess process

    onSuccess readChan (Just replIn, mreplOut, _, _) = do
        hSetBuffering replIn LineBuffering
        threadDelay 1000000
        hPutStrLn replIn loadString
        hPutStrLn replIn startString
        -- reloadBrowserOnBoot mreplOut
        forever $ do
            event <- atomically (readTChan readChan)
            putStrLn "-----------------------------"
            print event
            putStrLn "-----------------------------"
            hPutStrLn replIn loadString
            hPutStrLn replIn startString

    startString = "update"
    loadString = ":load app/DevelMain.hs"
    -- reloadBrowserOnBoot Nothing = return ()
    -- reloadBrowserOnBoot (Just replOut) = waitForServerBoot replOut >> reloadBrowser
    waitForServerBoot stdout = do
        l <- hGetLine stdout
        if "Devel application launched" `isInfixOf` pack l
            then return ()
            else waitForServerBoot stdout

shouldReload :: Event -> Bool
shouldReload event = not (or conditions)
  where
    fp = case event of
        Added filePath _ -> filePath
        Modified filePath _ -> filePath
        Removed filePath _ -> filePath
    -- p = case toText fp of
    --       Left filePath -> filePath
    --       Right filePath -> filePath
    -- fn = case toText (filename fp) of
    --         Left filePath -> filePath
    --         Right filePath -> filePath
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
    notInPath t = t `isInfixOf` (pack fp)
    notInFile t = t `isInfixOf` (pack $ takeBaseName fp)

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

-- browserSyncThread :: IO ()
-- browserSyncThread = do
--     putStrLn "Starting browser-sync..."
--     _ <- forkIO $ callCommand "browser-sync start --port 4000 --proxy localhost:3000"
--     putStrLn "browser-sync started at port 4000 proxying to localhost:3000"

-- reloadBrowser :: IO ()
-- reloadBrowser = do
--     putStrLn "Reloading browser..."
--     bracket (void $ callCommand "browser-sync reload --port 4000")
--         (const $ return ())
--         (const $ return ())
