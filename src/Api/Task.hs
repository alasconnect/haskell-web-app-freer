module Api.Task where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.Task
import Models.Task (TaskR, TaskId)
import Types
--------------------------------------------------------------------------------

type GetTasksApi
  = Get '[JSON] [TaskR]

type GetTaskApi
  =  Capture "task_id" TaskId
  :> Get '[JSON] (Maybe TaskR)

type CreateTaskApi
  =  ReqBody '[JSON] TaskR
  :> Post '[JSON] TaskR

type UpdateTaskApi
  =  ReqBody '[JSON] TaskR
  :> Put '[JSON] NoContent

type DeleteTaskApi
  =  Capture "task_id" TaskId
  :> Delete '[JSON] NoContent

type TaskApi
  =  "api"
  :> "v1"
  :> "tasks"
  :> (    GetTasksApi
     :<|> GetTaskApi
     :<|> CreateTaskApi
     :<|> UpdateTaskApi
     :<|> DeleteTaskApi
     )

data ApiTask r where
  TasksGet :: ApiTask [TaskR]
  TaskGet :: TaskId -> ApiTask (Maybe TaskR)
  TaskCreate :: TaskR -> ApiTask TaskR
  TaskUpdate :: TaskR -> ApiTask NoContent
  TaskDelete :: TaskId -> ApiTask NoContent

makeEffect ''ApiTask

tasksApi :: Proxy TaskApi
tasksApi = Proxy

tasksServer :: Members '[ApiTask] effs => ServerT TaskApi (Eff effs)
tasksServer =
       tasksGet
  :<|> taskGet
  :<|> taskCreate
  :<|> taskUpdate
  :<|> taskDelete

runApiTask :: Members '[DomainTask] effs => Eff (ApiTask ': effs) ~> Eff effs
runApiTask =
  interpret $ \case
    TasksGet -> getTasks
    TaskGet tid -> getTask tid
    TaskCreate t -> createTask t
    TaskUpdate t -> updateTask t >> return NoContent
    TaskDelete tid -> deleteTask tid >> return NoContent
