{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), Value(String), object, (.=))
import           Snap.Snaplet.PostgresqlSimple
import           Database.PostgreSQL.Simple.FromField

data Id = Id (T.Text) deriving (Show)

data Job = Job
  { jobId   :: Id,
    jobName :: T.Text,
    jobPipelineId :: Id
  } deriving (Show)

instance FromField Id where
  fromField f dat = Id <$> fromField f dat

instance FromRow Job where
  fromRow = Job <$> field
                <*> field
                <*> field

instance ToJSON Job where
   toJSON (Job (Id id) name (Id pipeline)) = object [ "id" .= id, "name" .= name, "pipeline" .= pipeline ]