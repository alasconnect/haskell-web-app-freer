module Domain.Task where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
--------------------------------------------------------------------------------
import qualified Database.Task as T
import Models.Task (Task, TaskId)
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

data DomainTask r where
  GetTasks :: DomainTask [Task]
  GetTask :: TaskId -> DomainTask (Maybe Task)
  CreateTask :: Task -> DomainTask Task
  UpdateTask :: Task -> DomainTask ()
  DeleteTask :: TaskId -> DomainTask ()
  GetUserTasks :: UserId -> DomainTask [Task]

makeEffect ''DomainTask

runDomainTask :: Members '[T.DatabaseTask] effs =>
  Eff (DomainTask ': effs) ~> Eff effs
runDomainTask =
  interpret $ \case
    GetTasks -> T.getTasks
    GetTask tid -> T.getTask tid
    CreateTask t -> T.createTask t
    UpdateTask t -> T.updateTask t
    DeleteTask tid -> T.deleteTask tid
    GetUserTasks uid -> T.getUserTasks uid
