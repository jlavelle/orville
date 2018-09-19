{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestDB where

import Control.Monad (void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Data.Convertible (convert)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as Postgres
import System.Environment (getEnv)

import Test.Tasty (TestTree, withResource)

import qualified Database.Orville as O
import qualified Database.Orville.Raw as ORaw

type TestPool = Pool Postgres.Connection

data Trace = Trace
  { tracePred :: O.QueryType -> Bool
  , traceRef :: IORef [(O.QueryType, String)]
  }

newtype TestMonad a = TestMonad
  { runTestMonad :: O.OrvilleT Postgres.Connection IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase IO
             , MonadThrow
             , MonadCatch
             , O.MonadOrville Postgres.Connection
             )

queryTrace ::
     (O.QueryType -> Bool) -> TestMonad a -> TestMonad [(O.QueryType, String)]
queryTrace queryPred action = do
  ref <- liftIO (newIORef [])
  void $ O.localOrvilleEnv (addTraceToEnv (Trace queryPred ref)) action
  reverse <$> liftIO (readIORef ref)

addTraceToEnv :: Trace -> O.OrvilleEnv conn -> O.OrvilleEnv conn
addTraceToEnv trace =
  O.aroundRunningQuery $ \queryType sql action -> do
    when
      (tracePred trace queryType)
      (modifyIORef (traceRef trace) ((queryType, sql) :))
    action

instance MonadBaseControl IO TestMonad where
  type StM TestMonad a = StM (O.OrvilleT Postgres.Connection IO) a
  liftBaseWith f =
    TestMonad $ liftBaseWith $ \runInBase -> f (\(TestMonad m) -> runInBase m)
  restoreM stm = TestMonad (restoreM stm)

reset :: O.SchemaDefinition -> O.Orville ()
reset schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
    -- I would like to use placeholders here, but postgres gives my a
    -- sql syntax error when I do :(
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef

withOrvilleRun :: ((forall a. TestMonad a -> IO a) -> TestTree) -> TestTree
withOrvilleRun mkTree = withDb (\pool -> mkTree (run pool))
  where
    run :: IO TestPool -> forall a. TestMonad a -> IO a
    run getPool (TestMonad action) = do
      pool <- getPool
      O.runOrville action (O.newOrvilleEnv pool)

withDb :: (IO TestPool -> TestTree) -> TestTree
withDb = withResource acquirePool destroyAllResources

acquirePool :: IO TestPool
acquirePool = do
  connString <- getEnv "TEST_CONN_STRING"
  createPool (Postgres.connectPostgreSQL' connString) HDBC.disconnect 1 60 1
