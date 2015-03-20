module Api.Services.JobService where

import Api.Generators (randomId)
import Api.Types (Id(..), Job(..))
import Api.Services.InterfaceTypes
import qualified Api.Services.Database as DB

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Snap.Core
import Snap.Extras.JSON
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data JobService = JobService { _pg :: Snaplet Postgres }

makeLenses ''JobService
type Route b c = Handler b JobService c



-- | Routes

jobRoutes :: [(B.ByteString, Handler b JobService ())]
jobRoutes = [("/", method GET routeListJobs),
             ("/", method POST routeCreateJob)]


jobServiceInit :: SnapletInit b JobService
jobServiceInit = makeSnaplet "job" "Job Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes jobRoutes
  return $ JobService pg


instance HasPostgres (Handler b JobService) where
  getPostgresState = with pg get



-- | Route handlers

routeListJobs :: Route b ()
routeListJobs = do
  response <- listJobs
  modifyResponse $ setResponseCode 200
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ response

routeCreateJob :: Route b ()
routeCreateJob = do
  request <- reqJSON
  response <- createJob request

  modifyResponse $ setResponseCode 201
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ response



-- | API implementation

listJobs :: Route b JobListResponse
listJobs = do
  jobs <- DB.listJobs
  return $ JobListResponse (map jobResponse jobs)

createJob :: JobCreationRequest -> Route b JobResponse
createJob (JobCreationRequest name pipeline) = do
  id <- liftIO $ randomId 'j'

  job <- DB.createJob id name (Id pipeline)
  return $ jobResponse job
