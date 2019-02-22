module Domain.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
--------------------------------------------------------------------------------
import qualified Database.User as U
import Models.User
import Types
import Util.Database.Pool
--------------------------------------------------------------------------------

data DomainUser r where
  GetUsers :: DomainUser [UserR]
  GetUser :: UserId -> DomainUser (Maybe UserR)
  CreateUser :: UserR -> DomainUser UserR
  UpdateUser :: UserR -> DomainUser ()
  DeleteUser :: UserId -> DomainUser ()

makeEffect ''DomainUser

runDomainUser :: Member U.DatabaseUser r
  => Eff (DomainUser ': r) a
  -> Eff r a
runDomainUser =
  interpret $ \case
    GetUsers -> withConn U.getUsers
    GetUser uid -> withConn $ U.getUser uid
    CreateUser u -> withConn $ U.createUser u
    UpdateUser u -> withConn $ U.updateUser u
    DeleteUser uid -> withConn $ U.deleteUser uid
