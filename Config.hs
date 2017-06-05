{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import Control.Applicative
import Data.Vector
import Data.Foldable
import Data.Yaml
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import Turtle hiding (Parser)

data Repo = Git { name :: FilePath, url :: FilePath }
          | Pass { url :: FilePath }
          | Vcsh { name :: FilePath, url :: FilePath }

instance ToJSON Repo where
  toJSON (Git {..}) = object [ "git" .= object [ "path" .= format fp name, "url" .= format fp url ] ]
  toJSON (Pass {..}) = object [ "pass" .= object [ "url" .= format fp url ] ]
  toJSON (Vcsh {..}) = object [ "vcsh" .= object [ "path" .= format fp name, "url" .= format fp url ] ]

parseFilePath :: Value -> Parser FilePath
parseFilePath = withText "file path" (pure . fromText)

instance FromJSON Repo where
  parseJSON = withObject "object"
    (\obj -> asum [ obj .: "git" >>= parseGit
                  , obj .: "pass" >>= parsePass
                  , obj .: "vcsh" >>= parseVcsh
                  ])
    where
      parseGit = withObject "Git repository" $ \obj -> do
        name <- obj .: "path" >>= parseFilePath
        url <- obj .: "url" >>= parseFilePath
        pure Git {..}
      parsePass = withObject "password-store repository" $ \obj -> do
        url <- obj .: "url" >>= parseFilePath
        pure Pass {..}
      parseVcsh = withObject "vcsh repository" $ \obj -> do
        name <- obj .: "path" >>= parseFilePath
        url <- obj .: "url" >>= parseFilePath
        pure Vcsh {..}

type Config = Vector Repo

readConfig :: IO Config
readConfig = do
  h <- home
  let config = h </> ".config" </> "repos.yaml"
  r <- decodeFileEither (encodeString config)
  case r of
    Right cfg -> pure cfg
    Left exc -> do
      putStrLn (prettyPrintParseException exc)
      exit (ExitFailure 1)
