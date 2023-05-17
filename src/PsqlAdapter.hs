module PsqlAdapter
  ( mkHandle,
    PsqlAdapterHandle,
    initDbValues,
  )
where

import Control.Exception (onException)
import Control.Exception.Base (mask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool, destroyResource, putResource, takeResource, withResource)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute, executeMany)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Int (Int64)
import Model (CoinFeature, CoinType)
import Text.Read.Lex (Number)
import TextShow (TextShow, fromText, showt)

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

initDbValues :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => PsqlAdapterHandle -> m ()
initDbValues (PsqlAdapterHandle pool) = do
  _ <- withResource pool initCoinTypes
  _ <- withResource pool initCoinFeatures

  pure ()
