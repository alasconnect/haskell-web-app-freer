{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Config where
  
--------------------------------------------------------------------------------
import Control.Lens
import Data.Text (Text)
import Dhall
--------------------------------------------------------------------------------

data AppConfig = AppConfig String Bool

data ConfigDb
  = ConfigDb
  { configDbHost :: String
  , configDbPort :: Integer
  , configDbName :: String
  , configDbUser :: String
  , configDbPassword :: String
  , configDbPoolStripes :: Integer
  , configDbPoolTime :: Integer
  , configDbPoolMax :: Integer
  } deriving (Generic, Show)

makeLensesWith camelCaseFields ''ConfigDb

instance Interpret ConfigDb

data Config
  = Config
  { configPort :: Integer
  , configEkgPort :: Integer
  , configDb :: ConfigDb
  } deriving (Generic, Show)

makeLensesWith camelCaseFields ''Config

instance Interpret Config
