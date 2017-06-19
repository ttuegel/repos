{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions.Git
  ( sync1, clone1
  , gitClone, gitPull, gitPush
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (empty, null)
import Prelude hiding (FilePath)
import Turtle hiding (Text, skip)

import Actions.Helpers
import Actions.Types

gitStatus :: MonadIO io => io ExitCode
gitStatus = do
  n <- fold (inproc "git" ["status", "--porcelain"] empty) countLines
  pure (if (n :: Int) > 0 then ExitFailure n else ExitSuccess)

gitPull :: MonadIO io => io ExitCode
gitPull =
  proc_ "git" ["pull", "--no-gpg-sign"] empty
  .&&.
  proc_ "git" ["submodule", "update", "--init"] empty

gitPush :: MonadIO io => io ExitCode
gitPush = proc_ "git" ["push", "--porcelain"] empty

status, pull, push :: MonadIO io => io ExitCode
status = gitStatus .||. cleanup "Please commit your changes"
pull = gitPull .||. cleanup "Please pull remote changes"
push = gitPush .||. cleanup "Please push local changes"

sync1 :: FilePath -> FilePath -> Targets -> IO ()
sync1 path _ targets

  | elem path targets || null targets =
      runManaged $ do
        home >>= pushd
        skipIfMissing path $ do
          announce path Nothing
          pushd path
          _ <- status .&&. pull .&&. push
          pure ()

  | otherwise = pure ()

gitClone :: MonadIO io => FilePath -> FilePath -> io ExitCode
gitClone path url =
  proc "git" ["clone", filePathArg url, filePathArg path] empty

clone1 :: FilePath -> FilePath -> Targets -> IO ()
clone1 path url targets
  | elem path targets =
    runManaged $ do
      home >>= pushd
      skipIfExists path $ void $ gitClone path url
  | otherwise = pure ()
