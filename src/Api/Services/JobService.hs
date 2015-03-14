{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.JobService where

import Api.Generators (randomId)
import Api.Types (Id(..), Job(Job))
import Control.Applicative
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Aeson (encode, FromJSON(..), Value(Object), (.:))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Snap.Core
import Snap.Extras.JSON
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data JobService = JobService { _pg :: Snaplet Postgres }

makeLenses ''JobService


jobRoutes :: [(B.ByteString, Handler b JobService ())]
jobRoutes = [("/", method POST createJob)]


{-
getJobs :: Handler b JobService ()
getJobs = do
  jobs <- query_ "SELECT * FROM job"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (jobs :: [Job])

-}

data JobCreationRequest = JobCreationRequest { jcrJobName :: T.Text, jcrJobPipeline :: T.Text }

instance FromJSON JobCreationRequest where
  parseJSON (Object v) = JobCreationRequest <$>
                         (v .: "name") <*>
                         (v .: "pipeline")

createJob :: Handler b JobService ()
createJob = do
  request <- reqJSON
  jobId@(Id jobIdRaw) <- liftIO $ randomId "job"

  let jobName    = jcrJobName request
      pipelineId = jcrJobPipeline request
      pipeline   = Id pipelineId
      job        = Job jobId jobName pipeline

  liftIO $ print job

  execute "INSERT INTO job (id, name, pipeline) VALUES (?, ?, ?)" (jobIdRaw, jobName, pipelineId)
  modifyResponse $ setResponseCode 201
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ job


jobServiceInit :: SnapletInit b JobService
jobServiceInit = makeSnaplet "job" "Job Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes jobRoutes
  return $ JobService pg


instance HasPostgres (Handler b JobService) where
  getPostgresState = with pg get