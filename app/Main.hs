module Main (main) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString.Char8 (pack)
import Processor
import PsqlAdapter (PsqlAdapterHandle, initDbValues, mkHandle)
import System.Environment (getEnv)

psqlHandle :: IO PsqlAdapterHandle
psqlHandle = do
  connString <- getEnv "CONNECTION_STRING"

  mkHandle $ pack connString

initApp :: PsqlAdapterHandle -> IO ()
initApp handle = do
  runStdoutLoggingT $ initDbValues handle

main :: IO ()
main = psqlHandle >>= initApp
