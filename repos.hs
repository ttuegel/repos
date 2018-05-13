{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Control.Applicative
import Control.Monad (join)
import Data.Semigroup
import Options.Applicative (ParserInfo, (<**>))
import qualified Options.Applicative as Options

import qualified Actions
import Config (readConfig)

actions :: ParserInfo (IO ())
actions = Options.info
          (parser <**> Options.helper)
          (Options.fullDesc <> Options.progDesc "Manage multiple repositories")
  where
    parser = Options.hsubparser (syncCmd <> cloneCmd)
    targets = many (Options.strArgument (Options.metavar "TARGET"))
    sync verb tgts =
      do
        cfg <- readConfig
        let ctx = Actions.Ctx { config = cfg, verbose = verb }
        Actions.sync ctx tgts
    syncCmd = Options.command "sync"
              (Options.info (sync <$> verbosity <*> targets)
               (Options.progDesc "Synchronize selected repositories"))
    clone verb tgts =
      do
        cfg <- readConfig
        let ctx = Actions.Ctx { config = cfg, verbose = verb }
        Actions.clone ctx tgts
    cloneCmd = Options.command "clone"
               (Options.info (clone <$> verbosity <*> targets)
                (Options.progDesc "Clone selected repositories"))
    verbosity = Options.flag False True (Options.long "verbose")

main :: IO ()
main = join (Options.execParser actions)
