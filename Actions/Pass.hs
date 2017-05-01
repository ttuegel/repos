{-# LANGUAGE OverloadedStrings #-}

module Actions.Pass where

import Prelude hiding (FilePath)
import Turtle

import Actions.Git (gitClone)
import Actions.Helpers
import Actions.Types

pull :: MonadIO io => io ExitCode
pull = proc_ "pass" ["git", "pull"] empty

push :: MonadIO io => io ExitCode
push = proc_ "pass" ["git", "push", "--porcelain"] empty

sync1 :: FilePath -> Targets -> IO ()
sync1 _ targets

  | elem ".password-store" targets || null targets =
      runManaged $ do
        home >>= pushd
        skipIfMissing path $ do
          announce path Nothing
          pushd path
          _ <-  pull .&&. push
          pure ()

  | otherwise = pure ()

  where path = ".password-store"

clone :: MonadIO io => FilePath -> io ExitCode
clone url = gitClone ".password-store" url

clone1 :: FilePath -> Targets -> IO ()
clone1 url targets
  | elem path targets =
    runManaged $ do
      home >>= pushd
      skipIfExists path $ void $ clone url
  | otherwise = pure ()
  where
    path = ".password-store"
