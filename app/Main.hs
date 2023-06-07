{-# LANGUAGE TupleSections #-}

module Main (main) where

import Conduit (ConduitT, mapC, mapM_C, runConduit, (.|))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Bifunctor (Bifunctor (second), first)
import Data.ByteString.Char8 (pack)
import Model (ClassifiedCoin, CoinIdentificationError, UnclassifiedCoin (title, unclassifiedCoinKey), classifyAs, identifyCoin, rp2CoinDefs)
import PsqlAdapter qualified as SQL
import System.Environment (getEnv)
import TextShow (showt)

psqlHandle :: IO SQL.PsqlAdapterHandle
psqlHandle = do
  connString <- getEnv "CONNECTION_STRING"

  SQL.mkHandle $ pack connString

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

handleClassificationResult :: SQL.PsqlAdapterHandle -> ClassificationResult SQL.LotsPk -> IO ()
handleClassificationResult handle =
  runStdoutLoggingT
    . either
      (SQL.markAsFailed handle . second showt)
      (SQL.markLotClassified handle)

runApp :: SQL.PsqlAdapterHandle -> IO ()
runApp handle =
  do
    runStdoutLoggingT $ SQL.initDbValues handle

    src <- SQL.inputSource handle

    let pipeline = src .| coinClassifierC .| mapM_C (handleClassificationResult handle)

    runConduit pipeline

main :: IO ()
main = psqlHandle >>= runApp
