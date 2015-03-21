module Api.Services.JobService where

import qualified Api.Generators as Gen
import Api.Types (Id(..))
import Api.Services.InterfaceTypes
import qualified Api.Services.Database as DB

import Control.Lens (makeLenses, set)
import Control.Monad (replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Control.Monad.Reader
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
             ("/", method POST routeCreateJob),
             ("/:job/page", method POST routeIngest)]


jobServiceInit :: Snaplet Postgres -> SnapletInit b JobService
jobServiceInit pg = makeSnaplet "job" "Job Service" Nothing $ do
  addRoutes jobRoutes
  return $ JobService pg


instance HasPostgres (Handler b JobService) where
  getPostgresState = with pg get
  setLocalPostgresState s = local (set (pg . snapletValue) s)



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


routeIngest :: Route b ()
routeIngest = do
  request <- reqJSON
  (Just job) <- getParam "job"

  response <- ingest (Id $ T.decodeUtf8 job) request

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
  id <- generateId 'j'
  job <- DB.createJob id name (Id pipeline)
  return $ jobResponse job


ingest :: Id -> IngestRequest -> Route b [Id]
ingest job (IngestRequest pages) = do
  ids <- replicateM pages (generateId 'p')
  forM_ ids $ DB.createPage job
  return ids


generateId :: Char -> Route b Id
generateId kind = liftIO $ Gen.randomId kind
