module Api.Types(
  Id(Id),

  Job(Job),
  jobId, jobName, jobPipeline,

  Metadata(..), KeyValue(..)
) where

import qualified Data.Text as T

data Id = Id (T.Text) deriving (Show)

data Job = Job
  { jobId   :: Id,
    jobName :: T.Text,
    jobPipeline :: Id
  } deriving (Show)


newtype Metadata = Metadata [KeyValue] deriving (Show)

data KeyValue = KeyValue { key :: T.Text, value :: T.Text } deriving (Show)
