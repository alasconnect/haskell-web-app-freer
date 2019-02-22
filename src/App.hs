module App where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Pool (Pool)
import Data.Proxy
import qualified Database.PostgreSQL.Simple as PGS
import Dhall
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
import System.Remote.Monitoring (forkServer)
--------------------------------------------------------------------------------
import Api.Task
import Api.User
import Config
import Database.User (DatabaseUser, runDatabaseUser)
import Database.Task (DatabaseTask, runDatabaseTask)
import Domain.User (DomainUser, runDomainUser)
import Domain.Task (DomainTask, runDomainTask)
import Types
import Util.Database.Pool
import Util.Logger
--------------------------------------------------------------------------------

type FullApi =
       UserApi
  :<|> TaskApi

type Effects = Eff
  '[ Logger
   , ApiUser
   , ApiTask
   , DomainUser
   , DomainTask
   , DatabaseUser
   , DatabaseTask
   , Reader AppContext
   , Reader (Pool PGS.Connection)
   , DatabasePool
   , IO
   ]

runEffects :: AppContext -> Effects a -> IO a
runEffects ctx = runM
  . runReader (view pool ctx)
  . runReader ctx
  . runDatabaseTask
  . runDatabaseUser
  . runDomainTask
  . runDomainUser
  . runApiTask
  . runApiUser
  . runDatabasePool
  . runLogger

fullApi :: Proxy FullApi
fullApi = Proxy

fullApiServer :: ServerT FullApi Effects
fullApiServer =
  usersServer :<|> tasksServer

server :: AppContext -> Server FullApi
server ctx =
  hoistServer fullApi (liftIO . runEffects ctx) fullApiServer

app :: AppContext -> Application
app ctx =
  serve fullApi (server ctx)

initCtx :: Config -> IO AppContext
initCtx cfg = do
  let c = PGS.defaultConnectInfo
           { PGS.connectHost = view (db . host) cfg
           , PGS.connectPort = fromIntegral . view (db . port) $ cfg
           , PGS.connectDatabase = view (db . name) cfg
           , PGS.connectUser = view (db . user) cfg
           , PGS.connectPassword = view (db . password) cfg
           }
      p = PgPoolConfig (fromIntegral . view (db . poolStripes) $ cfg)
                       (fromIntegral . view (db . poolTime) $ cfg)
                       (fromIntegral . view (db . poolMax) $ cfg)
      pc = PgConfig c p
  pool <- createPgPool pc
  return $ AppContext pool

exec :: AppConfig -> IO ()
exec (AppConfig f check) = do
  -- Load the config
  cfg <- input auto (pack f) :: IO Config
  -- Skip running the app if verifying config file
  when (not check) $ do
    -- Turn on EKG monitoring
    forkServer "localhost" (fromIntegral . view ekgPort $ cfg)
    -- Load config
    ctx <- initCtx cfg
    -- Run application
    W.run ((fromIntegral . view port) cfg) (app ctx)
