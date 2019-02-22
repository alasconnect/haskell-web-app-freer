{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Util.Database.Pool where

-------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Control.Monad.Trans.Resource
import Data.Pool (Pool, createPool, putResource, takeResource)
import Data.Time.Clock (NominalDiffTime)
import Database.PostgreSQL.Simple (ConnectInfo, Connection, close, connect)
-------------------------------------------------------------------------------

data PgPoolConfig
  = PgPoolConfig
  { _pgPoolConfigStripes :: Int
  , _pgPoolConfigResourceTime :: NominalDiffTime
  , _pgPoolConfigKillTime :: Int
  }

makeLensesWith camelCaseFields ''PgPoolConfig

data PgConfig
 = PgConfig
 { _pgConfigConnectInfo :: ConnectInfo
 , _pgConfigPgPoolConfig :: PgPoolConfig
 }

makeLensesWith camelCaseFields ''PgConfig

data DatabasePool r where
  WithConn :: (Connection -> Eff effs a) -> DatabasePool a

makeEffect ''DatabasePool

createPgPool :: PgConfig -> IO (Pool Connection)
createPgPool c =
  createPool (connect i) close s t k
  where
    s = view (pgPoolConfig . stripes) c
    t = view (pgPoolConfig . resourceTime) c
    k = view (pgPoolConfig . killTime) c
    i = view connectInfo c

-- see https://github.com/lexi-lambda/freer-simple/issues/20
bracket
  :: Member (ResourceT IO) r
  => IO a
  -> (a -> IO ())
  -> (a -> Eff r b)
  -> Eff r b
bracket alloc dealloc m = do
  (k, a) <- send $ allocate alloc dealloc
  r <- m a
  send $ release k
  return r

withResource
  :: Member (ResourceT IO) r
  => Pool a
  -> (a -> Eff r b)
  -> Eff r b
withResource p f =
  bracket (takeResource p) (uncurry (flip putResource)) (f . fst)

runDatabasePool ::
  ( Member (ResourceT IO) r
  , Member (Reader (Pool Connection)) r
  )
  => Eff (DatabasePool ': r) a
  -> Eff r a
runDatabasePool =
  interpret $ \case
    WithConn f -> do
      p <- ask
      send $ withResource p f
