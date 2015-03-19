module Api.Services.Database (
  createJob
) where

import Api.Types

import Control.Applicative
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField
import Snap.Core
import Snap.Snaplet.PostgresqlSimple

createJob :: (MonadSnap m, HasPostgres m) => Id -> T.Text -> Id -> m Job
createJob (Id id) name (Id pipeline) = do
  execute "INSERT INTO job (id, name, pipeline) VALUES (?, ?, ?)" (id, name, pipeline)
  return $ Job (Id id) name (Id pipeline)


instance FromField Id where
  fromField f dat = Id <$> fromField f dat

instance FromRow Job where
  fromRow = Job <$> field
                <*> field
                <*> field