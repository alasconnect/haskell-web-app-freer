module Api.Task where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.TH
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.Task
import Models.Task (Task, TaskId)
import Types
--------------------------------------------------------------------------------

type GetTasksApi
  = Get '[JSON] [Task]

type GetTaskApi
  =  Capture "task_id" TaskId
  :> Get '[JSON] (Maybe Task)

type CreateTaskApi
  =  ReqBody '[JSON] Task
  :> Post '[JSON] Task

type UpdateTaskApi
  =  ReqBody '[JSON] Task
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
  TasksGet :: ApiTask [Task]
  TaskGet :: TaskId -> ApiTask (Maybe Task)
  TaskCreate :: Task -> ApiTask Task
  TaskUpdate :: Task -> ApiTask NoContent
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
