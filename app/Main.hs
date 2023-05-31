{-# LANGUAGE TupleSections #-}

module Main (main) where

import Conduit (ConduitT, mapC, (.|))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Bifunctor (first)
import Data.ByteString.Char8 (pack)
import Model (ClassifiedCoin, CoinIdentificationError, UnclassifiedCoin (title, unclassifiedCoinKey), classifyAs, identifyCoin, rp2CoinDefs)
import PsqlAdapter (PsqlAdapterHandle, initDbValues, inputSource, mkHandle)
import System.Environment (getEnv)

psqlHandle :: IO PsqlAdapterHandle
psqlHandle = do
  connString <- getEnv "CONNECTION_STRING"

  mkHandle $ pack connString

type ClassificationResult a = Either (a, CoinIdentificationError) (ClassifiedCoin a)

tryClassifyCoin :: UnclassifiedCoin a -> ClassificationResult a
tryClassifyCoin un =
  classifyAs un <$> classifiedCoinE
  where
    coinTitle = title un
    coins = rp2CoinDefs
    classifiedCoinE = first (unclassifiedCoinKey un,) $ identifyCoin coins coinTitle

coinClassifierC :: (MonadIO m) => ConduitT (UnclassifiedCoin a) (ClassificationResult a) m ()
coinClassifierC = mapC tryClassifyCoin

initApp :: PsqlAdapterHandle -> IO ()
initApp handle =
  do
    runStdoutLoggingT $ initDbValues handle

    src <- inputSource handle

    let foo = src .| coinClassifierC
    pure ()

main :: IO ()
main = psqlHandle >>= initApp
