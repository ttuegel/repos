module Actions.Types where

import Prelude hiding (FilePath)
import Turtle (FilePath)

import Config

type Action = Config -> IO ()
type Targets = [FilePath]
