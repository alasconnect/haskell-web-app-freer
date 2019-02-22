{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

--------------------------------------------------------------------------------
import Control.Lens
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
--------------------------------------------------------------------------------
import Util.Database.Pool
--------------------------------------------------------------------------------

data AppContext
  = AppContext
  { appContextPool :: Pool Connection
  }

makeLensesWith camelCaseFields ''AppContext
