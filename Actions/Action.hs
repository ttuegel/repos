{-# LANGUAGE OverloadedStrings #-}

module Actions.Action
  ( Action, runAction, Ctx(..)
  , debug, info
  , module Control.Monad.Reader
  , module Control.Monad.Trans.Maybe
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Text.Lazy
import Formatting
import System.IO (stderr)

import Config

data Ctx =
  Ctx
  { config :: Config
  , verbose :: Bool
  }

type Action a = ReaderT Ctx (MaybeT IO) a

runAction :: Ctx -> Action a -> IO (Maybe a)
runAction ctx act = runMaybeT (runReaderT act ctx)

debug :: Format (Action ()) a -> a
debug f =
  runFormat f $ \b ->
    do
      verb <- Reader.asks verbose
      if verb
        then liftIO $ Text.Lazy.hPutStrLn stderr (Builder.toLazyText b)
        else return ()

info :: Format (Action ()) a -> a
info f = runFormat f (liftIO . Text.Lazy.hPutStrLn stderr . Builder.toLazyText)
