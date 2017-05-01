{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions.Git (sync1, clone1) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (traverse_)
import Filesystem.Path.CurrentOS hiding (empty, null)
import Prelude hiding (FilePath)
import Turtle hiding (Text, skip)

import Actions.Types
import Posix

proc_ :: MonadIO io => Text -> [Text] -> Shell Line -> io ExitCode
proc_ cmd args inp = do
  echo (unsafeTextToLine ("; " <> cmd <> " " <> T.intercalate " " args))
  proc cmd args inp

gitStatus :: MonadIO io => io ExitCode
gitStatus = do
  n <- fold (inproc "git" ["status", "--porcelain"] empty) countLines
  pure (if (n :: Int) > 0 then ExitFailure n else ExitSuccess)

gitPull :: MonadIO io => io ExitCode
gitPull =
  proc_ "git" ["pull"] empty
  .&&.
  proc_ "git" ["submodule", "update", "--init"] empty

gitPush :: MonadIO io => io ExitCode
gitPush = proc_ "git" ["push", "--porcelain"] empty

cleanup :: MonadIO io => Text -> io ExitCode
cleanup reason = do
  traverse_ echo (textToLines (format ("# "%s) reason))
  userShell <- getUserShell
  forkAndWait userShell []

skipIfMissing :: MonadIO io => FilePath -> io () -> io ()
skipIfMissing path go = do
  exists <- testdir path
  if exists then go else skip path "does not exist"

skipIfExists :: MonadIO io => FilePath -> io () -> io ()
skipIfExists path go = do
  exists <- testdir path
  if exists then skip path "path exists" else go

announce :: MonadIO io => FilePath -> Maybe Text -> io ()
announce path message = do
  let m pref = makeFormat (maybe "" ((<>) pref))
  printf ("# "%fp%(m ": ")%"\n") path message

skip :: MonadIO io => FilePath -> Text -> io ()
skip path reason =
  let message = format ("skipped ("%s%")") reason
  in announce path (Just message)

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

filePathArg :: FilePath -> Text
filePathArg = T.pack . encodeString

gitClone :: MonadIO io => FilePath -> FilePath -> io ExitCode
gitClone path url =
  proc "git" ["clone", filePathArg url, filePathArg path] empty

clone1 :: FilePath -> FilePath -> Targets -> IO ()
clone1 path url targets
  | elem path targets =
      skipIfExists path $ void $ gitClone path url
  | otherwise = pure ()
