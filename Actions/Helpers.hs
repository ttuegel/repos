{-# LANGUAGE OverloadedStrings #-}

module Actions.Helpers where

import Control.Applicative
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Foldl (Fold)
import qualified Control.Foldl as Foldl
import Control.Monad.IO.Class
import qualified Control.Monad.State.Strict as State
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import Formatting
import qualified Lens.Micro.Platform as Lens
import qualified Pipes
import Pipes.Concurrent (Input)
import qualified Pipes.Concurrent as Pipes
import qualified Pipes.Parse as Pipes
import qualified Pipes.Text.Encoding as Pipes.Text
import System.Directory
import System.Exit (ExitCode(..))
import System.IO (stderr)
import System.Process (showCommandForUser)

import Actions.Action
import Actions.Command

unrecoverable
  :: Text -> Command -> Input ByteString -> Async ExitCode -> Maybe r -> Action r
unrecoverable suggest cmd err aexit r =
  ReaderT $ \_ -> MaybeT $ do
    exit <- Async.wait aexit
    case exit of
      ExitSuccess -> return r
      ExitFailure e ->
        do
          dumpErrors err
          commandFailed cmd e
          hprint stderr (stext%"\n") suggest
          return Nothing

commandFailed :: Command -> Int -> IO ()
commandFailed cmd e =
  do
    let
      command = showCommandForUser (name cmd) (arguments cmd)
      message = "command `"%string%"' failed with exit code "%int%"\n"
    hprint stderr message command e

dumpErrors :: Input ByteString -> IO ()
dumpErrors err =
  do
    let
      producer = Pipes.fromInput err
      consumer = liftIO . ByteString.hPutStr stderr
    Pipes.runEffect $ Pipes.for producer consumer

skipIfMissing :: FilePath -> Action ()
skipIfMissing path = do
  exists <- liftIO $ doesDirectoryExist path
  if exists
    then
      do
        debug ("`"%string%"' is missing; skipping") path
        empty
    else return ()

skipIfExists :: FilePath -> Action ()
skipIfExists path = do
  exists <- liftIO $ doesDirectoryExist path
  if exists
    then return ()
    else
      do
        debug ("`"%string%"' exists; skipping") path
        empty

dropOutput :: Input ByteString -> Action ()
dropOutput _ = return ()

countLines :: Input ByteString -> Action Int
countLines out =
  do
    let
      counter = Foldl.purely Pipes.foldAll count
      parser = Lens.zoom Pipes.Text.utf8 counter
    Pipes.liftIO $ State.evalStateT parser (Pipes.fromInput out)
  where
    count :: Fold a Int
    count = Foldl.Fold (\i _ -> i + 1) 0 id
