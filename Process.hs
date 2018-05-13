module Process where

import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async as Async
import Data.ByteString (ByteString)
import Pipes ((>->))
import qualified Pipes
import Pipes.Concurrent (Input)
import qualified Pipes.ByteString as Pipes
import qualified Pipes.Concurrent as Pipes
import System.Exit (ExitCode)
import System.IO (hClose)
import qualified System.Process as Process


withProcess
  :: FilePath
  -> [String]
  -> Maybe FilePath
  -> (Input ByteString -> IO r)
  -> (Input ByteString -> Async ExitCode -> r -> IO r')
  -> IO r'
withProcess exe args cwd worker finalizer =
  Pipes.withSpawn Pipes.unbounded $ \(oOut, iOut) ->
  Pipes.withSpawn Pipes.unbounded $ \(oErr, iErr) ->
    do
      (inp, out, err, hnd) <- Process.runInteractiveProcess exe args cwd Nothing
      hClose inp
      Pipes.runEffect $ Pipes.fromHandle out >-> Pipes.toOutput oOut
      Pipes.runEffect $ Pipes.fromHandle err >-> Pipes.toOutput oErr
      exit <- async (Process.waitForProcess hnd)
      r <- worker iOut >>= finalizer iErr exit
      _ <- Async.wait exit
      return r

{-# INLINABLE withProcess #-}
