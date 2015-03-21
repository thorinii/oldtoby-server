module Api.Core where

import Api.Services.JobService(JobService, jobServiceInit)
import Api.Services.MetadataService(MetadataService, metadataServiceInit)
import qualified Data.ByteString.Char8 as B
import Control.Lens (makeLenses)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple


data Api = Api { _pg :: Snaplet Postgres,
                 _jobService :: Snaplet JobService,
                 _metadataService :: Snaplet MetadataService }

makeLenses ''Api


apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200


apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
       pg <- nestSnaplet "pg" pg pgsInit
       js <- nestSnaplet "job" jobService (jobServiceInit pg)
       ms <- nestSnaplet "metadata" metadataService (metadataServiceInit pg)
       addRoutes apiRoutes
       return $ Api pg js ms
