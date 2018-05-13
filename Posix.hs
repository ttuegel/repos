{-# LANGUAGE LambdaCase #-}

module Posix
  ( getUserShell
  , forkAndWait
  ) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Process
import System.Posix.User

getUserEntry :: MonadIO io => io UserEntry
getUserEntry = liftIO $ getRealUserID >>= getUserEntryForID

getUserShell :: MonadIO io => io FilePath
getUserShell = userShell <$> getUserEntry

forkAndWait :: MonadIO io => FilePath -> [Text] -> Maybe FilePath -> io ExitCode
forkAndWait cmd args cwd = liftIO $ do
  pid <- forkProcess $ do
    forM_ cwd setCurrentDirectory
    executeFile cmd True (T.unpack <$> args) Nothing
  getProcessStatus True True pid >>= \case
    Nothing -> pure (ExitFailure 1)
    Just st ->
      case st of
        Exited ec -> pure ec
        Terminated _ _ -> pure (ExitFailure 1)
        Stopped _ -> pure (ExitFailure 1)
