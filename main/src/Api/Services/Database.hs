module Api.Services.Database (
  listJobs,
  createJob,
  createPage,
  pushMetadata,
  getMetadata
) where

import Api.Types

import Control.Applicative
import Control.Monad
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


pushMetadata :: HasPostgres m => Id -> T.Text -> Metadata -> m ()
pushMetadata page stage (Metadata metadata) = do
  forM_ metadata $ pushKeyValue page stage
  return ()

pushKeyValue :: HasPostgres m => Id -> T.Text -> KeyValue -> m ()
pushKeyValue (Id page) stage (KeyValue key value) = do
  execute "INSERT INTO metadata (page, stage, dkey, dvalue) VALUES (?, ?, ?, ?)" (page, stage, key, value)
  return ()


getMetadata :: HasPostgres m => Id -> m Metadata
getMetadata  (Id page) = do
  keyvalues <- query "SELECT dkey, dvalue FROM metadata WHERE page = ?" (Only page)
  return $ Metadata keyvalues


instance FromField Id where
  fromField f dat = Id <$> fromField f dat

instance FromRow Job where
  fromRow = Job <$> field
                <*> field
                <*> field

instance FromRow KeyValue where
  fromRow = KeyValue <$> field
                     <*> field
                                                    --(\(a, b) -> (a :: T.Text, b :: T.Text))
