module Database.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
--------------------------------------------------------------------------------
import Models.User
import Types
--------------------------------------------------------------------------------

data DatabaseUser r where
  GetUsers :: DatabaseUser [User]
  GetUser :: UserId -> DatabaseUser (Maybe User)
  CreateUser :: User -> DatabaseUser User
  UpdateUser :: User -> DatabaseUser ()
  DeleteUser :: UserId -> DatabaseUser ()

makeEffect ''DatabaseUser

runDatabaseUser :: Eff (DatabaseUser ': effs) ~> Eff effs
runDatabaseUser =
  interpret $ \case
    GetUsers -> undefined
    GetUser uid -> undefined
    CreateUser u -> undefined
    UpdateUser u -> undefined
    DeleteUser uid -> undefined
