{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions
  ( sync, clone
  , module Actions.Action
  ) where

import Data.Foldable

import Actions.Action
import qualified Actions.Git as Git
import qualified Actions.Pass as Pass
import qualified Actions.Vcsh as Vcsh
import Config

sync :: Ctx -> [FilePath] -> IO ()
sync ctx targets = for_ (config ctx) (\r -> runAction ctx $ sync1 r targets)

sync1 :: Repo -> [FilePath] -> Action ()
sync1 (Git {..}) = Git.sync1 name url
sync1 (Pass {..}) = Pass.sync1 url
sync1 (Vcsh {..}) = Vcsh.sync1 name url

clone :: Ctx -> [FilePath] -> IO ()
clone ctx targets = for_ (config ctx) (\r -> runAction ctx $ clone1 r targets)

clone1 :: Repo -> [FilePath] -> Action ()
clone1 (Git {..}) = Git.clone1 name url
clone1 (Pass {..}) = Pass.clone1 url
clone1 (Vcsh {..}) = Vcsh.clone1 name url
