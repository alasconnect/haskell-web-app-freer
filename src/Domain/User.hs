module Domain.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
--------------------------------------------------------------------------------
import qualified Database.User as U
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

data DomainUser r where
  GetUsers :: DomainUser [User]
  GetUser :: UserId -> DomainUser (Maybe User)
  CreateUser :: User -> DomainUser User
  UpdateUser :: User -> DomainUser ()
  DeleteUser :: UserId -> DomainUser ()

makeEffect ''DomainUser

runDomainUser :: Members '[U.DatabaseUser] effs =>
  Eff (DomainUser ': effs) ~> Eff effs
runDomainUser =
  interpret $ \case
    GetUsers -> U.getUsers
    GetUser uid -> U.getUser uid
    CreateUser u -> U.createUser u
    UpdateUser u -> U.updateUser u
    DeleteUser uid -> U.deleteUser uid
