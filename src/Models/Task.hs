module Models.Task where

--------------------------------------------------------------------------------
import Data.Aeson hiding (fieldLabelModifier)
import qualified Data.Aeson.TH as ATH (fieldLabelModifier, deriveJSON)
import Control.Lens
import Data.Aeson
import Data.Tagged (Tagged(..), untag)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import GHC.Generics
import Servant.API
--------------------------------------------------------------------------------
import Models.User (UserId)
--------------------------------------------------------------------------------

data TaskIdTag
type TaskId = Tagged TaskIdTag Int

mkTaskId :: Int -> TaskId
mkTaskId = Tagged

instance ToHttpApiData TaskId where
  toUrlPiece = pack . show . untag
instance FromHttpApiData TaskId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . mkTaskId . fromInteger $ v
      Left e       -> Left . pack $ e

data TaskNameTag
type TaskName = Tagged TaskNameTag Text

mkTaskName :: Text -> TaskName
mkTaskName = Tagged

data TaskR
  = TaskR
  { _taskId     :: TaskId
  , _taskUserId :: UserId
  , _taskName   :: TaskName
  } deriving (Generic, Show)

makeLenses ''TaskR
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = drop 6 } ''TaskR
