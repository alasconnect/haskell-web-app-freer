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
  GetTasks :: DatabaseTask [TaskR]
  GetTask :: TaskId -> DatabaseTask (Maybe TaskR)
  CreateTask :: TaskR -> DatabaseTask TaskR
  UpdateTask :: TaskR -> DatabaseTask ()
  DeleteTask :: TaskId -> DatabaseTask ()
  GetUserTasks :: UserId -> DatabaseTask [TaskR]

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
