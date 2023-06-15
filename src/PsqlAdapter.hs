{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PsqlAdapter
  ( mkHandle,
    PsqlAdapterHandle,
    initDbValues,
    inputSource,
    markAsFailed,
    markLotClassified,
    LotsPk,
  )
where

import Conduit (ConduitT)
import Control.Concurrent.STM.TMChan (closeTMChan, newTMChanIO, writeTMChan)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.Conduit.TMChan (sourceTMChan)
import Data.Pool (Pool, createPool, takeResource, withResource)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, FromRow, Query, close, connectPostgreSQL, execute, executeMany, forEach_)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Conc (atomically, forkIO)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Model (ClassifiedCoin (classifiedCoinKey, coin), Coin (coinDef), CoinDef (coinType, year), CoinFeature, CoinType, RawGrading (RawGrading), UnclassifiedCoin (..), features, unYear)
import TextShow (TextShow, showt)

newtype PsqlAdapterHandle = PsqlAdapterHandle (Pool Connection)

mkHandle :: ByteString -> IO PsqlAdapterHandle
mkHandle connString = do
  let createConn = connectPostgreSQL connString
  let destroyConn = close

  c <- createConn
  destroyConn c

  pool <- createPool createConn destroyConn 1 10 5

  return $ PsqlAdapterHandle pool

describeEnum :: forall a. (Enum a, Bounded a, TextShow a) => Proxy a -> [(Int, Text)]
describeEnum _ =
  (\(x :: a) -> (fromEnum x, showt x)) <$> [minBound :: a .. maxBound :: a]

initCoinTypes :: (MonadIO m, MonadLogger m) => Connection -> m Int64
initCoinTypes conn = do
  logInfoN "Initializing coin types"
  changedRows <-
    liftIO $
      executeMany
        conn
        [sql|
        INSERT INTO "coin_types" (id, name) VALUES (?,?) ON CONFLICT DO NOTHING
      |]
        (describeEnum (Proxy :: Proxy CoinType))
  logInfoN [i|Initialization of coin types: #{changedRows} rows changed|]
  pure changedRows

initCoinFeatures :: (MonadIO m, MonadLogger m) => Connection -> m Int64
initCoinFeatures conn = do
  logInfoN "Initializing coin features"
  changedRows <-
    liftIO $
      executeMany
        conn
        [sql|
          INSERT INTO "coin_features" (id, name) VALUES (?,?) ON CONFLICT DO NOTHING
        |]
        (describeEnum (Proxy :: Proxy CoinFeature))

  logInfoN [i|Initialization of coin features: #{changedRows} rows changed|]
  pure changedRows

mkPgSource :: (MonadIO m) => ((r -> IO ()) -> IO ()) -> IO (ConduitT () r m ())
mkPgSource action = do
  chan <- newTMChanIO
  _ <-
    forkIO $ do
      action $ atomically . writeTMChan chan
      atomically $ closeTMChan chan
  pure $ sourceTMChan chan

sourceQuery_ :: (FromRow r, MonadIO m) => Connection -> Query -> IO (ConduitT () r m ())
sourceQuery_ conn q = mkPgSource $ forEach_ conn q

newtype LotsPk = LotsPk {unLotsPk :: Text}
  deriving (FromRow, Generic, Show)

instance FromField LotsPk where
  fromField f bs = LotsPk <$> fromField f bs

instance FromField RawGrading where
  fromField f bs = RawGrading <$> fromField f bs

instance FromRow (UnclassifiedCoin LotsPk)

inputSource :: PsqlAdapterHandle -> IO (ConduitT () (UnclassifiedCoin LotsPk) IO ())
inputSource (PsqlAdapterHandle pool) = do
  -- TODO: This should be released... sometime
  -- Probably return release handle and destroy in the upper layer
  (conn, _) <- takeResource pool

  sourceQuery_
    conn
    [sql|
      SELECT url as unclassifiedCoinKey, title, condition FROM "lots" WHERE "classification" IS NULL AND "classification_state" = 0 ORDER BY "date"
    |]

markAsFailed :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => PsqlAdapterHandle -> (LotsPk, Text) -> m ()
markAsFailed (PsqlAdapterHandle pool) (pk, reason) = do
  logDebugN [i|Marking #{pk} as failed in PSQL because of: '#{reason}'|]

  _ <- withResource pool $ \conn ->
    liftIO $
      execute
        conn
        [sql|
        UPDATE "lots" SET "classification_state" = -1, "classification_error" = ? WHERE "url"=?
      |]
        (reason, unLotsPk pk)

  logDebugN [i|Marked #{pk} as failed in PSQL|]

markLotClassified :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => PsqlAdapterHandle -> ClassifiedCoin LotsPk -> m ()
markLotClassified (PsqlAdapterHandle pool) c = do
  logDebugN [i|Marking as classified:  #{classifiedCoinKey c}'|]

  let cDef = coinDef $ coin c

  _ <- withResource pool $ \conn ->
    liftIO $
      execute
        conn
        [sql|
        UPDATE "lots" SET "classification_state" = 1, "classification_error" = NULL, "classification" = ?, "identified_year" = ? WHERE "url"=?
      |]
        (fromEnum $ coinType cDef, unYear $ year cDef, unLotsPk $ classifiedCoinKey c)

  let featureLots = fmap ((unLotsPk $ classifiedCoinKey c,) . fromEnum) $ features $ coin c

  featuresInserted <-
    withResource pool $ \conn ->
      liftIO $
        executeMany
          conn
          [sql|
          INSERT INTO "lots_features" (lot_url, feature_id) VALUES (?,?) ON CONFLICT DO NOTHING
        |]
          featureLots

  logDebugN [i|Inserted #{featuresInserted} features|]

-- how does this monad base control work? :O
initDbValues :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => PsqlAdapterHandle -> m ()
initDbValues (PsqlAdapterHandle pool) = do
  _ <- withResource pool initCoinTypes
  _ <- withResource pool initCoinFeatures

  pure ()
