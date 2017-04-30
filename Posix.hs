{-# LANGUAGE LambdaCase #-}

module Posix
  ( getUserShell
  , forkAndWait
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty, null)
import Prelude hiding (FilePath)
import System.Posix.Process
import System.Posix.User
import Turtle hiding (Text, skip)

getUserEntry :: MonadIO io => io UserEntry
getUserEntry = liftIO $ getRealUserID >>= getUserEntryForID

getUserShell :: MonadIO io => io FilePath
getUserShell = (decodeString . userShell) <$> getUserEntry

forkAndWait :: MonadIO io => FilePath -> [Text] -> io ExitCode
forkAndWait cmd args = liftIO $ do
  pid <- forkProcess $ do
    executeFile (encodeString cmd) True (T.unpack <$> args) Nothing
  getProcessStatus True True pid >>= \case
    Nothing -> pure (ExitFailure 1)
    Just st ->
      case st of
        Exited ec -> pure ec
        Terminated _ _ -> pure (ExitFailure 1)
        Stopped _ -> pure (ExitFailure 1)
