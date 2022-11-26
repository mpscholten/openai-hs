module OpenAI.Client
  ( -- * Basics
    ApiKey,
    OpenAIClient,
    makeOpenAIClient,
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    OpenAIList (..),

    -- * Engine
    EngineId (..),
    Engine (..),
    listEngines,
    getEngine,

    -- * Text completion
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultTextCompletionCreate,
    completeText,

    -- * Embeddings
    EmbeddingCreate (..),
    Embedding (..),
    createEmbedding,

    -- * Fine tunes
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),
    createFineTune,
    listFineTunes,
    getFineTune,
    cancelFineTune,
    listFineTuneEvents,

    -- * Searching
    SearchResult (..),
    SearchResultCreate (..),
    searchDocuments,

    -- * File API
    FileCreate (..),
    File (..),
    FileId (..),
    FileHunk (..),
    SearchHunk (..),
    ClassificationHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),
    createFile,
    deleteFile,

    -- * Answer API
    getAnswer,
    AnswerReq (..),
    AnswerResp (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import OpenAI.Api
import OpenAI.Client.Internal.Helpers
import OpenAI.Resources
import Servant.API
import Servant.Client
import qualified Servant.Multipart.Client as MP

-- | Your OpenAI API key. Can be obtained from the OpenAI dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data OpenAIClient = OpenAIClient
  { scBasicAuthData :: BasicAuthData,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'OpenAIClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeOpenAIClient ::
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  OpenAIClient
makeOpenAIClient k = OpenAIClient (BasicAuthData "" (T.encodeUtf8 k))

api :: Proxy OpenAIApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.openai.com" 443 ""




completeText' :: BasicAuthData ->  EngineId ->  TextCompletionCreate -> ClientM  TextCompletion
completeText :: OpenAIClient ->  EngineId ->  TextCompletionCreate -> IO (Either ClientError  TextCompletion)
completeText sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (completeText' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) openaiBaseUrl)

searchDocuments' :: BasicAuthData ->  EngineId ->  SearchResultCreate -> ClientM  (OpenAIList SearchResult)
searchDocuments :: OpenAIClient ->  EngineId ->  SearchResultCreate -> IO (Either ClientError  (OpenAIList SearchResult))
searchDocuments sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (searchDocuments' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) openaiBaseUrl)

createEmbedding' :: BasicAuthData ->  EngineId ->  EmbeddingCreate -> ClientM  (OpenAIList Embedding)
createEmbedding :: OpenAIClient ->  EngineId ->  EmbeddingCreate -> IO (Either ClientError  (OpenAIList Embedding))
createEmbedding sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (createEmbedding' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) openaiBaseUrl)

createFineTune' :: BasicAuthData ->  FineTuneCreate -> ClientM  FineTune
createFineTune :: OpenAIClient ->  FineTuneCreate -> IO (Either ClientError  FineTune)
createFineTune sc a = runRequest (scMaxRetries sc) 0 $ runClientM (createFineTune' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

listFineTunes' :: BasicAuthData -> ClientM  (OpenAIList FineTune)
listFineTunes :: OpenAIClient -> IO (Either ClientError  (OpenAIList FineTune))
listFineTunes sc = runRequest (scMaxRetries sc) 0 $ runClientM (listFineTunes' (scBasicAuthData sc)) (mkClientEnv (scManager sc) openaiBaseUrl)

getFineTune' :: BasicAuthData ->  FineTuneId -> ClientM  FineTune
getFineTune :: OpenAIClient ->  FineTuneId -> IO (Either ClientError  FineTune)
getFineTune sc a = runRequest (scMaxRetries sc) 0 $ runClientM (getFineTune' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

cancelFineTune' :: BasicAuthData ->  FineTuneId -> ClientM  FineTune
cancelFineTune :: OpenAIClient ->  FineTuneId -> IO (Either ClientError  FineTune)
cancelFineTune sc a = runRequest (scMaxRetries sc) 0 $ runClientM (cancelFineTune' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

listFineTuneEvents' :: BasicAuthData ->  FineTuneId -> ClientM  (OpenAIList FineTuneEvent)
listFineTuneEvents :: OpenAIClient ->  FineTuneId -> IO (Either ClientError  (OpenAIList FineTuneEvent))
listFineTuneEvents sc a = runRequest (scMaxRetries sc) 0 $ runClientM (listFineTuneEvents' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

listEngines' :: BasicAuthData -> ClientM  (OpenAIList Engine)
listEngines :: OpenAIClient -> IO (Either ClientError  (OpenAIList Engine))
listEngines sc = runRequest (scMaxRetries sc) 0 $ runClientM (listEngines' (scBasicAuthData sc)) (mkClientEnv (scManager sc) openaiBaseUrl)

getEngine' :: BasicAuthData ->  EngineId -> ClientM  Engine
getEngine :: OpenAIClient ->  EngineId -> IO (Either ClientError  Engine)
getEngine sc a = runRequest (scMaxRetries sc) 0 $ runClientM (getEngine' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

createFile :: OpenAIClient -> FileCreate -> IO (Either ClientError File)
createFile sc rfc =
  do
    bnd <- MP.genBoundary
    createFileInternal sc (bnd, rfc)

createFileInternal' :: BasicAuthData ->  (BSL.ByteString, FileCreate) -> ClientM  File
createFileInternal :: OpenAIClient ->  (BSL.ByteString, FileCreate) -> IO (Either ClientError  File)
createFileInternal sc a = runRequest (scMaxRetries sc) 0 $ runClientM (createFileInternal' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

deleteFile' :: BasicAuthData ->  FileId -> ClientM  FileDeleteConfirmation
deleteFile :: OpenAIClient ->  FileId -> IO (Either ClientError  FileDeleteConfirmation)
deleteFile sc a = runRequest (scMaxRetries sc) 0 $ runClientM (deleteFile' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

getAnswer' :: BasicAuthData ->  AnswerReq -> ClientM  AnswerResp
getAnswer :: OpenAIClient ->  AnswerReq -> IO (Either ClientError  AnswerResp)
getAnswer sc a = runRequest (scMaxRetries sc) 0 $ runClientM (getAnswer' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)


( listEngines'
    :<|> getEngine'
    :<|> completeText'
    :<|> searchDocuments'
    :<|> createEmbedding'
  )
  :<|> (createFileInternal' :<|> deleteFile')
  :<|> getAnswer'
  :<|> ( createFineTune'
           :<|> listFineTunes'
           :<|> getFineTune'
           :<|> cancelFineTune'
           :<|> listFineTuneEvents'
         ) =
    client api
