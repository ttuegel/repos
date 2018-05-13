module Actions.Command where

data Command =
  Command
  { name :: FilePath
  , arguments :: [String]
  , working :: Maybe FilePath
  }
