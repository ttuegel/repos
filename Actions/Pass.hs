{-# LANGUAGE OverloadedStrings #-}

module Actions.Pass where

import Control.Concurrent.Async (Async)
import Data.ByteString (ByteString)
import Pipes.Concurrent (Input)
import System.Exit (ExitCode)

import Actions.Action
import Actions.Command
import qualified Actions.Git as Git
import Actions.Helpers
import qualified Process

withPass
  :: [String]  -- ^ arguments
  -> (Input ByteString -> Action a)  -- ^ worker
  -> (Command -> Input ByteString -> Async ExitCode -> Maybe a -> Action a)
  -> Action a
withPass args worker recovery =
  ReaderT $ \ctx -> MaybeT $ do
    Process.withProcess "git" args Nothing
      (\out -> runAction ctx $ worker out)
      (\err aex res -> runAction ctx $ recovery cmd err aex res)
  where
    cmd =
      Command
      { name = "git"
      , arguments = args
      , working = Nothing
      }

pull :: Action ()
pull =
  withPass ["git", "pull"] dropOutput
  (unrecoverable "pass: could not pull in remote changes")

push :: Action ()
push =
  withPass ["git", "push", "--porcelain"] dropOutput
  (unrecoverable "pass: could not push local changes")

sync1 :: FilePath -> [FilePath] -> Action ()
sync1 _ targets

  | elem path targets || null targets =
      do
        skipIfMissing path
        pull
        push

  | otherwise = pure ()

  where
    path = ".password-store"

clone :: FilePath -> Action ()
clone url = Git.clone ".password-store" url

clone1 :: FilePath -> [FilePath] -> Action ()
clone1 url targets
  | elem path targets =
      do
        skipIfExists path
        clone url

  | otherwise = pure ()
  where
    path = ".password-store"
