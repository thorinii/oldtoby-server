module Api.Types(
  Id(Id),
  Job(Job),

  jobId, jobName, jobPipeline
) where

import qualified Data.Text as T

data Id = Id (T.Text) deriving (Show)

data Job = Job
  { jobId   :: Id,
    jobName :: T.Text,
    jobPipeline :: Id
  } deriving (Show)
