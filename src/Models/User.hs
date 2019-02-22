module Models.User where

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

data UserIdTag
type UserId = Tagged UserIdTag Int

mkUserId :: Int -> UserId
mkUserId = Tagged

instance ToHttpApiData UserId where
  toUrlPiece = pack . show . untag
instance FromHttpApiData UserId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . mkUserId . fromInteger $ v
      Left e       -> Left . pack $ e

data UserNameTag
type UserName = Tagged UserNameTag Text

mkUserName :: Text -> UserName
mkUserName = Tagged

data UserR
  = UserR
  { _userId       :: UserId
  , _userUserName :: UserName
  } deriving (Generic, Show)

makeLenses ''UserR
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = drop 6 } ''UserR
