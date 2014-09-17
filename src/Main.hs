module Main where

import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>), isAbsolute, takeDirectory)

import Cabal (findCabalFile)
import Daemonize (daemonize)
import Server (startServer, createListenSocket)

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath path = if isAbsolute path then return path else do
    dir <- getCurrentDirectory
    return $ dir </> path


defaultSocketFile :: FilePath
defaultSocketFile = ".ghc-mod-socket.sock"


main :: IO ()
main = do
    args <- getArgs
    let sockPath = case args of
          []     -> Nothing
          [sock] -> Just sock
          _      -> error "bad arguments. supply: [socket]"
    dir  <- getCurrentDirectory
    mCabalFile <- findCabalFile dir >>= traverse absoluteFilePath

    let defaultSocketPath = maybe "" takeDirectory mCabalFile </> defaultSocketFile
    let sock = fromMaybe defaultSocketPath sockPath

    s <- createListenSocket sock
    daemonize True $ startServer sock (Just s)
