module Sentry where

import Control.Exception (SomeException)
import Network.Wai       (Request, rawPathInfo, requestHeaderHost)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))
import qualified Data.ByteString.UTF8 as C (toString)
import System.Log.Raven (initRaven, register, silentFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import Network.Wai.Handler.Warp (defaultOnException)


sentryOnException :: Maybe Request -> SomeException -> IO()
sentryOnException mRequest exception = do
  sentryService <- initRaven
    "https://29f3f50ca0fa4aff84252aaaf2712123@o431619.ingest.sentry.io/5383215"
    id
    sendRecord
    silentFallback
  register
    sentryService
    "rentaLogger"
    Error
    (formatMessage mRequest exception)
    (recordUpdate mRequest exception)
  defaultOnException mRequest exception

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing _exception record        = record
recordUpdate (Just request) _exception record = record
  { srCulprit = Just $ C.toString $ rawPathInfo request
  , srServerName = fmap C.toString $ requestHeaderHost request
  }
  
formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception        = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request
