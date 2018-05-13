{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions.Git
    ( sync1, clone1
    , clone, pull, push, status
    , withGit
    ) where

import Control.Applicative
import Control.Concurrent.Async (Async)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString (ByteString)
import Formatting
import Pipes.Concurrent (Input)
import System.Exit (ExitCode(..))

import Actions.Action
import Actions.Command
import Actions.Helpers
import qualified Process

withGit
  :: [String]  -- ^ arguments
  -> Maybe FilePath  -- ^ working directory
  -> (Input ByteString -> Action a)  -- ^ worker
  -> (Command -> Input ByteString -> Async ExitCode -> Maybe a -> Action a)
  -> Action a
withGit args cwd worker recovery =
  ReaderT $ \ctx -> MaybeT $ do
    Process.withProcess "git" args cwd
      (\out -> runAction ctx $ worker out)
      (\err aex res -> runAction ctx $ recovery cmd err aex res)
  where
    cmd =
      Command
      { name = "git"
      , arguments = args
      , working = cwd
      }

status :: FilePath -> Action ()
status cwd =
  withGit ["status", "--porcelain"] (Just cwd) worker
  (unrecoverable "Unrecorded changes")
  where
    worker :: Input ByteString -> Action ()
    worker out =
      do
        n <- countLines out
        if n > 0
          then empty
          else return ()

pull :: FilePath -> Action ()
pull cwd =
  do
    withGit ["pull", "--no-gpg-sign"] (Just cwd) dropOutput
      (unrecoverable "Could not pull remote changes")
    withGit ["submodule", "update", "--init"] (Just cwd) dropOutput
      (unrecoverable "Could not update submodules")

push :: FilePath -> Action ()
push cwd =
  withGit ["push", "--porcelain"] (Just cwd) dropOutput
  (unrecoverable "Could not push local changes")

sync1 :: FilePath -> FilePath -> [FilePath] -> Action ()
sync1 path _ targets

  | elem path targets || null targets =
      do
        skipIfMissing path
        status path
        pull path
        push path

  | otherwise = pure ()


clone :: FilePath -> FilePath -> Action ()
clone path url =
  withGit ["clone", url, path] Nothing dropOutput
  (unrecoverable suggest)
  where
    suggest =
      sformat ("git: failed to clone `"%string%"' into `"%string%"'") url path

clone1 :: FilePath -> FilePath -> [FilePath] -> Action ()
clone1 path url targets

  | elem path targets =
      do
        skipIfExists path
        clone path url

  | otherwise = pure ()
