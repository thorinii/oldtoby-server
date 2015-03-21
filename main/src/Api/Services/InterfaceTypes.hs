module Api.Services.InterfaceTypes (
  JobCreationRequest(..),
  IngestRequest(..),
  PageMetadataPush(..),

  JobResponse,
  JobListResponse(..),

  jobResponse
) where

import Api.Types

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T


data JobCreationRequest = JobCreationRequest { jcrJobName :: T.Text,
                                               jcrJobPipeline :: T.Text } deriving (Show)
data IngestRequest = IngestRequest { irPages :: Int } deriving (Show)

data PageMetadataPush = PageMetadataPush Id T.Text Metadata deriving (Show)

data JobResponse = JobResponse Id T.Text Id T.Text deriving (Show)
data JobListResponse = JobListResponse [JobResponse] deriving (Show)


jobResponse :: Job -> JobResponse
jobResponse (Job id@(Id i) name pipeline) = JobResponse id name pipeline (buildUrl ["job", i])


buildUrl :: [T.Text] -> T.Text
buildUrl pieces = "/" `T.append` T.intercalate "/" pieces


-- | JSON conversion

instance FromJSON JobCreationRequest where
  parseJSON (Object v) = JobCreationRequest <$>
                           (v .: "name") <*>
                           (v .: "pipeline")
  parseJSON _          = mzero


instance FromJSON IngestRequest where
  parseJSON (Object v) = IngestRequest <$>
                           (v .: "pages")
  parseJSON _          = mzero


instance FromJSON PageMetadataPush where
  parseJSON (Object v) = PageMetadataPush <$>
                           (v .: "page") <*>
                           (v .: "stage") <*>
                           (v .: "metadata")
  parseJSON _          = mzero


instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> parseMetadata v
    where
      parseMetadata o' = return $ map toKeyValue (M.toList o')
      toKeyValue (k, String v) = KeyValue k v
      toKeyValue _             = error "Values must be strings"
  parseJSON _          = mzero


instance FromJSON Id where
  parseJSON (String s) = return $ Id s
  parseJSON _          = mzero



instance ToJSON Id where
  toJSON (Id id) = toJSON id


instance ToJSON Metadata where
  toJSON (Metadata d) = toJSON $ kvmap d
    where
      kvmap keyvalues = (M.fromList . map unKeyValue) keyvalues
      unKeyValue (KeyValue k v) = (k, v)


instance ToJSON JobResponse where
  toJSON (JobResponse (Id id) name (Id pipeline) url) =
    object [ "id" .= id,
             "name" .= name,
             "pipeline" .= pipeline,
             "url" .= url ]


instance ToJSON JobListResponse where
  toJSON (JobListResponse jobs) = toJSON $ toJSON <$> jobs
