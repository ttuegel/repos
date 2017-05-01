{-# LANGUAGE OverloadedStrings #-}

module Actions.Helpers where

import qualified Data.Text as T
import Data.Foldable (traverse_)
import Filesystem.Path.CurrentOS hiding (empty, null)
import Prelude hiding (FilePath)
import Turtle hiding (skip)

import Posix

proc_ :: MonadIO io => Text -> [Text] -> Shell Line -> io ExitCode
proc_ cmd args inp = do
  printf ("; "%s%" "%s%"\n") cmd (T.intercalate " " args)
  proc cmd args inp

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

filePathArg :: FilePath -> Text
filePathArg = T.pack . encodeString
