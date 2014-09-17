module Server where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Network
import System.Directory
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process


createListenSocket :: FilePath -> IO Socket
createListenSocket socketPath =
    listenOn (UnixSocket socketPath)

startServer :: FilePath -> Maybe Socket -> IO ()
startServer socketPath mbSock = do
    case mbSock of
        --Nothing -> bracket (createListenSocket socketPath) cleanup (n
        --Just sock -> run sock `finally` cleanup sock
        Nothing -> bracket (createListenSocket socketPath) (const restart) run
        Just sock -> run sock `finally` restart
    where
    restart = startServer socketPath mbSock

    cleanup :: Socket -> IO ()
    cleanup sock = do
        sClose sock
        removeSocketFile

    removeSocketFile :: IO ()
    removeSocketFile = do
        -- Ignore possible error if socket file does not exist
        _ <- tryJust (guard . isDoesNotExistError) $ removeFile socketPath
        return ()


run :: Socket -> IO ()
run sock = do
    (Just hin, Just hout, _, p) <- createProcess (proc "ghc-modi" [])
        { std_in  = CreatePipe
        , std_out = CreatePipe
        --, std_err = CreatePipe
        }
    hSetBuffering hin LineBuffering
    hSetBuffering hout LineBuffering
    forever $ do
        (h, _, _) <- accept sock
        hSetBuffering h LineBuffering
        msg <- hGetLine h
        hPutStrLn hin msg
        ls <- fix $ \loop -> do
            o <- hGetLine hout
            case o of
                "OK" -> return []
                s    -> (s:) <$> loop
        hPutStrLn h (unlines ls)
        hClose h
