module Api.Core where

import Api.Services.JobService(JobService, jobServiceInit)
import qualified Data.ByteString.Char8 as B
import Control.Lens (makeLenses)
import Snap.Core
import Snap.Snaplet


data Api = Api { _jobService :: Snaplet JobService }

makeLenses ''Api


apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200


apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
       js <- nestSnaplet "job" jobService jobServiceInit
       addRoutes apiRoutes
       return $ Api js