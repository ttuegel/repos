{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Dhall
import qualified Dhall as Dhall
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Turtle.Prelude

data Repo = Git { name :: FilePath
                , url :: FilePath
                }
          | Pass
  deriving (Generic)

instance Interpret FilePath where
  autoWith _ = (fromText . TL.toStrict) <$> text

instance Interpret Repo

type Config = Vector Repo

readConfig :: IO Config
readConfig = do
  h <- home
  let config = h </> ".config" </> "repos"
  Dhall.input auto =<< TL.readFile (encodeString config)
