module Main where

import Data.Semigroup
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import Options.Applicative

import Actions
import Config (readDefaultConfig)

actions :: ParserInfo Action
actions = info
          (parser <**> helper)
          (fullDesc <> progDesc "Manage multiple repositories")
  where
    parser = hsubparser (syncCmd <> cloneCmd)
    repos = map (fromText . T.pack) <$> many (strArgument (metavar "REPO"))
    syncCmd = command "sync"
              (info (sync <$> repos)
               (progDesc "Synchronize selected repositories"))
    cloneCmd = command "clone"
               (info (clone <$> repos)
                (progDesc "Clone selected repositories"))

main :: IO ()
main = do
  act <- execParser actions
  cfg <- readDefaultConfig
  act cfg
