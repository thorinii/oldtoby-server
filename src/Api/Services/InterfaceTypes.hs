module Api.Services.InterfaceTypes (
  JobCreationRequest(..),
  JobResponse,
  JobListResponse(..),

  jobResponse
) where

import Api.Types (Id(..), Job(..))

import Control.Applicative
import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), object, (.:), (.=))
import qualified Data.Text as T

data JobCreationRequest = JobCreationRequest { jcrJobName :: T.Text,
                                               jcrJobPipeline :: T.Text }

data JobResponse = JobResponse Id T.Text Id T.Text

data JobListResponse = JobListResponse [JobResponse]


jobResponse :: Job -> JobResponse
jobResponse (Job id@(Id i) name pipeline) = JobResponse id name pipeline (buildUrl ["job", i])



instance FromJSON JobCreationRequest where
  parseJSON (Object v) = JobCreationRequest <$>
                         (v .: "name") <*>
                         (v .: "pipeline")
  parseJSON _          = empty


instance ToJSON Id where
   toJSON (Id id) = toJSON id

instance ToJSON JobResponse where
   toJSON (JobResponse (Id id) name (Id pipeline) url) =
      object [ "id" .= id,
               "name" .= name,
               "pipeline" .= pipeline,
               "url" .= url ]

instance ToJSON JobListResponse where
   toJSON (JobListResponse jobs) = toJSON $ toJSON <$> jobs



buildUrl :: [T.Text] -> T.Text
buildUrl pieces = "/" `T.append` T.intercalate "/" pieces
