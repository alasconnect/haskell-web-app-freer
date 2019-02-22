{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where
  
-------------------------------------------------------------------------------
import Control.Lens (Lens', makeLenses)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Schema.Tables
-------------------------------------------------------------------------------
import Models.Task
import Models.User
-------------------------------------------------------------------------------

data UserT f
  = User
  { _utId       :: Columnar f UserId
  , _utUserName :: Columnar f UserName
  } deriving (Generic)
makeLenses ''UserT
instance Beamable UserT
type User = UserT Identity
deriving instance Show User
deriving instance Eq User
deriving instance Ord User
instance Table UserT where
  data PrimaryKey UserT f =
    UserKey { _userKeyId :: Columnar f UserId } deriving (Generic)
  primaryKey = UserKey . _utId
makeLenses 'UserKey
instance Beamable (PrimaryKey UserT)
type UserKey = PrimaryKey UserT Identity
deriving instance Show UserKey
deriving instance Eq UserKey
deriving instance Ord UserKey

data TaskT f
  = Task
  { _ttId       :: Columnar f TaskId
  , _ttUserId   :: Columnar f UserId
  , _ttTaskName :: Columnar f TaskName
  } deriving (Generic)
makeLenses ''TaskT
instance Beamable TaskT
type Task = TaskT Identity
deriving instance Show Task
deriving instance Eq Task
deriving instance Ord Task
instance Table TaskT where
  data PrimaryKey TaskT f =
    TaskKey { _taskKeyId :: Columnar f TaskId } deriving (Generic)
  primaryKey = TaskKey . _ttId
makeLenses 'TaskKey
instance Beamable (PrimaryKey TaskT)
type TaskKey = PrimaryKey TaskT Identity
deriving instance Show TaskKey
deriving instance Eq TaskKey
deriving instance Ord TaskKey

data MyDb f
  = MyDb
  {
  -- User
    _mydbUser :: f (TableEntity UserT)
  -- Task
  , _mydbTask :: f (TableEntity TaskT)
  } deriving (Generic)
makeLenses ''MyDb
instance Database Postgres MyDb

myDb :: DatabaseSettings Postgres MyDb
myDb = defaultDbSettings
