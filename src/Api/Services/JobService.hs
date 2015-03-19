module Api.Services.JobService where

import Api.Generators (randomId)
import Api.Types (Id(..), Job(..))
import qualified Api.Services.Database as DB

import Control.Applicative
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Aeson (encode, FromJSON(..), ToJSON(..), Value(Object), object, (.:), (.=))
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
jobRoutes = [("/", method POST routeCreateJob)]


jobServiceInit :: SnapletInit b JobService
jobServiceInit = makeSnaplet "job" "Job Service" Nothing $ do
  pg <- nestSnaplet "pg" pg pgsInit
  addRoutes jobRoutes
  return $ JobService pg


instance HasPostgres (Handler b JobService) where
  getPostgresState = with pg get



-- | Route handlers

routeCreateJob :: Route b ()
routeCreateJob = do
  request <- reqJSON
  response <- createJob request

  modifyResponse $ setResponseCode 201
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ response


-- | API implementation

createJob :: JobCreationRequest -> Route b JobResponse
createJob (JobCreationRequest name pipeline) = do
  jobId <- liftIO $ randomId "job"

  job <- DB.createJob jobId name (Id pipeline)
  return $ jobResponse job

{-
getJobs :: Handler b JobService ()
getJobs = do
  jobs <- query_ "SELECT * FROM job"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (jobs :: [Job])

-}



-- | Request/response data structures

data JobCreationRequest = JobCreationRequest { jcrJobName :: T.Text,
                                               jcrJobPipeline :: T.Text }

data JobResponse = JobResponse { jrId :: Id,
                                 jrName :: T.Text,
                                 jrPipeline :: Id }

jobResponse :: Job -> JobResponse
jobResponse (Job id name pipeline) = JobResponse id name pipeline



instance FromJSON JobCreationRequest where
  parseJSON (Object v) = JobCreationRequest <$>
                         (v .: "name") <*>
                         (v .: "pipeline")
  parseJSON _          = empty


instance ToJSON JobResponse where
   toJSON (JobResponse (Id id) name (Id pipeline)) = object [ "id" .= id,
                                                              "name" .= name,
                                                              "pipeline" .= pipeline ]
