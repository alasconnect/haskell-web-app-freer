module Api.User where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.Task
import Domain.User
import Models.Task (Task)
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

type GetUsersApi
  = Get '[JSON] [User]

type GetUserApi
  =  Capture "user_id" UserId
  :> Get '[JSON] (Maybe User)

type CreateUserApi
  =  ReqBody '[JSON] User
  :> Post '[JSON] User

type UpdateUserApi
  =  ReqBody '[JSON] User
  :> Put '[JSON] NoContent

type DeleteUserApi
  =  Capture "user_id" UserId
  :> Delete '[JSON] NoContent

type GetUserTasksApi
  =  Capture "user_id" UserId
  :> Get '[JSON] [Task]

type GetUserAndTasksApi
  =  "tasks"
  :> Capture "user_id" UserId
  :> Get '[JSON] (Maybe User, [Task])

type UserApi
  =  "api"
  :> "v1"
  :> "users"
  :> (    GetUsersApi
     :<|> GetUserApi
     :<|> CreateUserApi
     :<|> UpdateUserApi
     :<|> DeleteUserApi
     :<|> GetUserTasksApi
     :<|> GetUserAndTasksApi
     )

data ApiUser r where
  UsersGet :: ApiUser [User]
  UserGet :: UserId -> ApiUser (Maybe User)
  UserCreate :: User -> ApiUser User
  UserUpdate :: User -> ApiUser NoContent
  UserDelete :: UserId -> ApiUser NoContent
  UserTasks :: UserId -> ApiUser [Task]
  UserAndTasks :: UserId -> ApiUser (Maybe User, [Task])

makeEffect ''ApiUser

usersApi :: Proxy UserApi
usersApi = Proxy

usersServer :: Members '[ApiUser] effs => ServerT UserApi (Eff effs)
usersServer =
       usersGet
  :<|> userGet
  :<|> userCreate
  :<|> userUpdate
  :<|> userDelete
  :<|> userTasks
  :<|> userAndTasks

runApiUser :: Members '[Reader AppContext, DomainUser, DomainTask] effs
  => Eff (ApiUser ': effs) ~> Eff effs
runApiUser =
  interpret $ \case
    UsersGet -> getUsers
    UserGet uid -> getUser uid
    UserCreate u -> createUser u
    UserUpdate u -> updateUser u >> return NoContent
    UserDelete uid -> deleteUser uid >> return NoContent
    UserAndTasks uid -> do
      ctx :: AppContext <- ask
      u <- getUser uid
      ts <- getUserTasks uid
      return (u, ts)
