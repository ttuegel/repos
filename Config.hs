{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Config where

import Control.Applicative
import qualified Control.Exception
import qualified Data.ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text.Encoding
import qualified Data.Text
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Dhall (Type(..))
import qualified Dhall
import Dhall.Context (Context)
import qualified Dhall.Context
import Dhall.Core (Expr(..), Normalizer)
import qualified Dhall.Core hiding (Type)
import qualified Dhall.Import
import Dhall.Parser (Src(..))
import qualified Dhall.Parser
import Dhall.TypeCheck (X)
import qualified Dhall.TypeCheck
import Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FilePath
import Formatting.Buildable (Buildable(..))
import Prelude hiding (FilePath)
import Text.Trifecta.Delta (Delta(..))
import Turtle hiding (Parser, bytes)


data Repo = Git { name :: FilePath, url :: FilePath }
          | Pass { url :: FilePath }
          | Vcsh { name :: FilePath, url :: FilePath }
  deriving (Eq, Show)


asText :: Expr Src X -> Maybe Text
asText = Dhall.extract Dhall.strictText


git :: Type Repo
git =
    Dhall.Type {..}
  where
    expected = expected_git
    extract =
        \case
          RecordLit fields ->
            do
              name <- FilePath.fromText <$> (Map.lookup "name" fields >>= asText)
              url <- FilePath.fromText <$> (Map.lookup "url" fields >>= asText)
              pure Git {..}
          _ -> Nothing

expected_git :: Expr s X
expected_git =
    Record
    [
      ("name", Text),
      ("url", Text)
    ]


pass :: Type Repo
pass =
    Dhall.Type {..}
  where
    expected = expected_pass
    extract =
        \case
          RecordLit fields ->
            do
              url <- FilePath.fromText <$> (Map.lookup "url" fields >>= asText)
              pure Pass {..}
          _ -> Nothing

expected_pass :: Expr s X
expected_pass =
    Record
    [
      ("url", Text)
    ]


vcsh :: Type Repo
vcsh =
    Dhall.Type {..}
  where
    expected = expected_vcsh
    extract =
        \case
          RecordLit fields ->
            do
              name <- FilePath.fromText <$> (Map.lookup "name" fields >>= asText)
              url <- FilePath.fromText <$> (Map.lookup "url" fields >>= asText)
              pure Vcsh {..}
          _ -> Nothing

expected_vcsh :: Expr s X
expected_vcsh =
    Record
    [
      ("name", Text),
      ("url", Text)
    ]



repo :: Type Repo
repo =
    Dhall.Type {..}
  where
    expected =
        Union
        [
          ("git", Dhall.expected git),
          ("pass", Dhall.expected pass),
          ("vcsh", Dhall.expected vcsh)
        ]

    extract =
        \case
          UnionLit constr val _
            | constr == "git" -> Dhall.extract git val
            | constr == "pass" -> Dhall.extract pass val
            | constr == "vcsh" -> Dhall.extract vcsh val
          _ -> Nothing


type Config = Vector Repo


list :: [a] -> [a]
list = id


typeOfBuiltins :: Context (Expr Src X)
typeOfBuiltins =
    (foldr1 (.) . list)
    [
      Dhall.Context.insert "git" typeOf_git,
      Dhall.Context.insert "pass" typeOf_pass,
      Dhall.Context.insert "vcsh" typeOf_vcsh
    ]
    Dhall.Context.empty
  where
    typeOf_git = Pi "_" (Dhall.expected git) (Dhall.expected repo)
    typeOf_vcsh = Pi "_" (Dhall.expected vcsh) (Dhall.expected repo)
    typeOf_pass = Pi "_" (Dhall.expected pass) (Dhall.expected repo)


builtins :: Normalizer X
builtins =
    \case
      App (Var v) a
        | v == "git" ->
            pure (UnionLit "git" a (Map.delete "git" constrs))
        | v == "pass" ->
            pure (UnionLit "pass" a (Map.delete "pass" constrs))
        | v == "vcsh" ->
            pure (UnionLit "vcsh" a (Map.delete "vcsh" constrs))
      _ -> Nothing
  where
    constrs =
        [
          ("git", expected_git),
          ("pass", expected_pass),
          ("vcsh", expected_vcsh)
        ]


readDefaultConfig :: IO Config
readDefaultConfig = readConfig "./.repos.dhall"


readConfig :: FilePath -> IO Config
readConfig configFile =
  do
    configExists <- testfile configFile
    if configExists
      then readConfig1
      else pure Vector.empty
  where
    (Type { expected, extract }) = Dhall.vector repo

    parse =
      do
        txt <- readTextFile configFile
        configFile' <- realpath configFile
        let
          fileNameForHuman =
              (Data.Text.unpack . either id id)
              (FilePath.toText configFile')
        case Dhall.Parser.exprFromText fileNameForHuman txt of
          Left exn -> Dhall.detailed (Control.Exception.throwIO exn)
          Right parsed -> pure parsed

    readConfig1 =
      do
        parsed <- parse
        loaded <- Dhall.Import.loadWithContext typeOfBuiltins builtins parsed
        typed <-
          case Dhall.TypeCheck.typeWith typeOfBuiltins loaded of
            Left err ->
              Dhall.detailed (Control.Exception.throwIO err)
            Right _ ->
              pure loaded
        let
          normalized :: Expr Src X
          normalized = Dhall.Core.normalizeWith builtins typed
        case extract normalized of
          Just config -> pure config
          Nothing ->
              Dhall.detailed (Control.Exception.throwIO Dhall.InvalidType)
