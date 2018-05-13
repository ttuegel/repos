{-# LANGUAGE OverloadedStrings #-}

module Actions.Vcsh where

import Control.Applicative
import Control.Concurrent.Async (Async)
import Data.ByteString (ByteString)
import Formatting
import Pipes.Concurrent (Input)
import System.Exit (ExitCode)
import System.FilePath

import Actions.Action
import Actions.Command
import Actions.Helpers
import qualified Process

withVcsh
  :: [String]  -- ^ arguments
  -> (Input ByteString -> Action a)  -- ^ worker
  -> (Command -> Input ByteString -> Async ExitCode -> Maybe a -> Action a)
  -> Action a
withVcsh args worker recovery =
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

status :: FilePath -> Action ()
status target =
  withVcsh [target, "status", "--porcelain"] worker
  (unrecoverable "Unrecorded changes")
  where
    worker out = countLines out >>= \n -> if n > 0 then empty else return ()

pull :: FilePath -> Action ()
pull target =
  withVcsh [target, "pull"] dropOutput
  (unrecoverable "Could not pull remote changes")

push :: FilePath -> Action ()
push target =
  withVcsh [target, "push", "--porcelain"] dropOutput
  (unrecoverable "Could not push local changes")

sync1 :: FilePath -> FilePath -> [FilePath] -> Action ()
sync1 target _ targets

  | elem (".vcsh" </> target) targets || null targets =
      do
        skipIfMissing path
        status target
        pull target
        push target

  | otherwise = pure ()

  where
    path = ".config/vcsh/repo.d" </> target <.> "git"

clone :: FilePath -> FilePath -> Action ()
clone target url =
  withVcsh ["clone", url, target] dropOutput (unrecoverable suggest)
  where
    suggest = sformat ("vcsh: failed to clone `"%string%"'") url

clone1 :: FilePath -> FilePath -> [FilePath] -> Action ()
clone1 target url targets

  | elem path targets =
      do
        skipIfExists path
        clone target url

  | otherwise = pure ()

  where
    path = ".config/vcsh/repo.d" </> target <.> "git"
