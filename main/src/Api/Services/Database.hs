module Api.Services.Database (
  listJobs,
  createJob,
  createPage
) where

import Api.Types

import Control.Applicative
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField
import Snap.Snaplet.PostgresqlSimple


listJobs :: HasPostgres m => m [Job]
listJobs = query_ "SELECT * FROM job"


createJob :: HasPostgres m => Id -> T.Text -> Id -> m Job
createJob (Id id) name (Id pipeline) = do
  execute "INSERT INTO job (id, name, pipeline) VALUES (?, ?, ?)" (id, name, pipeline)
  return $ Job (Id id) name (Id pipeline)


createPage :: HasPostgres m => Id -> Id -> m ()
createPage (Id job) (Id id) = do
  execute "INSERT INTO page (id, job) VALUES (?, ?)" (id, job)
  return ()



instance FromField Id where
  fromField f dat = Id <$> fromField f dat

instance FromRow Job where
  fromRow = Job <$> field
                <*> field
                <*> field
