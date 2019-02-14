module Database.Task where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
--------------------------------------------------------------------------------
import Models.Task
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

data DatabaseTask r where
  GetTasks :: DatabaseTask [Task]
  GetTask :: TaskId -> DatabaseTask (Maybe Task)
  CreateTask :: Task -> DatabaseTask Task
  UpdateTask :: Task -> DatabaseTask ()
  DeleteTask :: TaskId -> DatabaseTask ()
  GetUserTasks :: UserId -> DatabaseTask [Task]

makeEffect ''DatabaseTask

runDatabaseTask :: Eff (DatabaseTask ': effs) ~> Eff effs
runDatabaseTask =
  interpret $ \case
    GetTasks -> undefined
    GetTask tid -> undefined
    CreateTask t -> undefined
    UpdateTask t -> undefined
    DeleteTask tid -> undefined
    GetUserTasks uid -> undefined
