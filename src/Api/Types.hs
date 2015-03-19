module Api.Types(
  Id(Id),
  Job(Job),

  jobId, jobName, jobPipeline
) where

import qualified Data.Text as T
import Data.Aeson (ToJSON(toJSON), object, (.=))

data Id = Id (T.Text) deriving (Show)

data Job = Job
  { jobId   :: Id,
    jobName :: T.Text,
    jobPipeline :: Id
  } deriving (Show)


instance ToJSON Job where
   toJSON (Job (Id id) name (Id pipeline)) = object [ "id" .= id,
                                                      "name" .= name,
                                                      "pipeline" .= pipeline ]