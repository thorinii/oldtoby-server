module Api.Services.MetadataService where

import Api.Types
import Api.Services.InterfaceTypes
import qualified Api.Services.Database as DB

import Control.Lens (makeLenses, set)
import Control.Monad (forM_)
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

data MetadataService = MetadataService { _pg :: Snaplet Postgres }

makeLenses ''MetadataService
type Route b c = Handler b MetadataService c



-- | Routes

metadataRoutes :: [(B.ByteString, Route b ())]
metadataRoutes = [("/", method POST routePushBulkMetadata),
                  ("/:page", method GET routeGetMetadata)]


metadataServiceInit :: Snaplet Postgres -> SnapletInit b MetadataService
metadataServiceInit pg = makeSnaplet "metadata" "Metadata Service" Nothing $ do
  --pg <- nestSnaplet "pg" pg pgsInit
  addRoutes metadataRoutes
  return $ MetadataService pg


instance HasPostgres (Handler b MetadataService) where
  getPostgresState = with pg get
  setLocalPostgresState s = local (set (pg . snapletValue) s)



-- | Route handlers

routePushBulkMetadata :: Route b ()
routePushBulkMetadata = do
  push <- reqJSON
  forM_ push pushMetadata

  modifyResponse $ setResponseCode 201



routeGetMetadata :: Route b ()
routeGetMetadata = do
  (Just page) <- getParam "page"
  metadata <- getMetadata (Id $ T.decodeUtf8 page)
  modifyResponse $ setResponseCode 200
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ metadata


-- | API implementation

pushMetadata :: PageMetadataPush -> Route b ()
pushMetadata (PageMetadataPush page stage metadata) = DB.pushMetadata page stage metadata


getMetadata :: Id -> Route b Metadata
getMetadata page = DB.getMetadata page
