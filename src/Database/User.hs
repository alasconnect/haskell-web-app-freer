module Database.User where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple (Connection)
--------------------------------------------------------------------------------
import Database
import Models.User
import Types
import Util.Database.Pool
--------------------------------------------------------------------------------

data DatabaseUser r where
  GetUsers :: Connection -> DatabaseUser [UserR]
  GetUser :: UserId -> Connection -> DatabaseUser (Maybe UserR)
  CreateUser :: UserR -> Connection -> DatabaseUser UserR
  UpdateUser :: UserR -> Connection -> DatabaseUser ()
  DeleteUser :: UserId -> Connection -> DatabaseUser ()

makeEffect ''DatabaseUser

toUserR :: User -> UserR
toUserR u =
  UserR (view utId u) (view utUserName u)

queryAll :: Q PgSelectSyntax MyDb s (UserT (QExpr PgExpressionSyntax s))
queryAll =
  all_ (myDb ^. mydbUser)

runDatabaseUser ::
  ( LastMember IO r
  , Member IO r
  )
  => Eff (DatabaseUser ': r) a
  -> Eff r a
runDatabaseUser =
  interpret $ \case
    GetUsers conn -> do
      vs <- send $ runBeamPostgres conn $
        runSelectReturningList $ select $ queryAll
      return . fmap toUserR $ vs
    GetUser conn uid -> undefined
    CreateUser conn u -> undefined
    UpdateUser conn u -> undefined
    DeleteUser conn uid -> undefined
