{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions
  ( sync, clone
  , module Actions.Types
  ) where

import Data.Foldable
import Prelude hiding (FilePath)

import qualified Actions.Git as Git
import Actions.Types
import Config

sync :: Targets -> Action
sync targets cfg = for_ cfg (\r -> sync1 r targets)

sync1 :: Repo -> Targets -> IO ()
sync1 (Git {..}) = Git.sync1 name url

clone :: Targets -> Action
clone targets cfg = for_ cfg (\r -> clone1 r targets)

clone1 :: Repo -> Targets -> IO ()
clone1 (Git {..}) = Git.clone1 name url
