{-# LANGUAGE ConstraintKinds #-}

module App where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import qualified Data.Map as Map
import Data.Proxy
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
--------------------------------------------------------------------------------
import Api.Task
import Api.User
import Database.User (DatabaseUser, runDatabaseUser)
import Database.Task (DatabaseTask, runDatabaseTask)
import Domain.User (DomainUser, runDomainUser)
import Domain.Task (DomainTask, runDomainTask)
import Types
--------------------------------------------------------------------------------

type FullApi =
       UserApi
  :<|> TaskApi

type Effects = Eff
  '[ ApiUser
   , ApiTask
   , DomainUser
   , DomainTask
   , DatabaseUser
   , DatabaseTask
   , Reader AppContext
   , Handler
   ]

runEffects :: AppContext -> Effects a -> Handler a
runEffects ctx = runM
  . runReader ctx
  . runDatabaseTask
  . runDatabaseUser
  . runDomainTask
  . runDomainUser
  . runApiTask
  . runApiUser

fullApi :: Proxy FullApi
fullApi = Proxy

fullApiServer :: ServerT FullApi Effects
fullApiServer =
  usersServer :<|> tasksServer

server :: AppContext -> Server FullApi
server ctx =
  hoistServer fullApi (runEffects ctx) fullApiServer

app :: AppContext -> Application
app ctx =
  serve fullApi (server ctx)

exec :: IO ()
exec = do
  let ctx = AppContext FakeConn
  W.run 8080 (app ctx)
