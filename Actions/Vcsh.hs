{-# LANGUAGE OverloadedStrings #-}

module Actions.Vcsh where

import Prelude hiding (FilePath)
import Turtle

import Actions.Helpers
import Actions.Types

status, pull, push :: MonadIO io => FilePath -> io ExitCode
status name = vcshStatus name .||. cleanup "Please commit your changes"
pull name = vcshPull name .||. cleanup "Please pull remote changes"
push name = vcshPush name .||. cleanup "Please push local changes"

vcshStatus :: MonadIO io => FilePath -> io ExitCode
vcshStatus name = do
  n <- fold (inproc "vcsh" [filePathArg name, "status", "--porcelain"] empty) countLines
  pure (if (n :: Int) > 0 then ExitFailure n else ExitSuccess)

vcshPull :: MonadIO io => FilePath -> io ExitCode
vcshPull name = proc_ "vcsh" [filePathArg name, "pull"] empty

vcshPush :: MonadIO io => FilePath -> io ExitCode
vcshPush name = proc_ "vcsh" [filePathArg name, "push", "--porcelain"] empty

sync1 :: FilePath -> FilePath -> Targets -> IO ()
sync1 name _ targets

  | elem (".vcsh" </> name) targets || null targets =
      runManaged $ do
        home >>= pushd
        skipIfMissing path $ do
          announce (".vcsh" </> name) Nothing
          _ <-  status name .&&. pull name .&&. push name
          pure ()

  | otherwise = pure ()

  where
    path = ".config/vcsh/repo.d" </> name <.> "git"

clone :: MonadIO io => FilePath -> FilePath -> io ExitCode
clone name url =
  proc_ "vcsh" ["clone", filePathArg url, filePathArg name] empty

clone1 :: FilePath -> FilePath -> Targets -> IO ()
clone1 name url targets

  | elem path targets =
    runManaged $ do
      home >>= pushd
      skipIfExists path $ void $ clone name url

  | otherwise = pure ()

  where
    path = ".config/vcsh/repo.d" </> name <.> "git"

