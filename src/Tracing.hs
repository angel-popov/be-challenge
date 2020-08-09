{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Tracing where
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import System.Clock
import System.Random
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as BS
data Span = Span {
  duration :: Integer,
  name :: String,
  resource:: String,
  service :: String,
  span_id :: Integer,
  start :: Integer,
  trace_id :: Integer
  } deriving (Show,Generic, FromJSON, ToJSON)

publish :: Span -> IO()
publish s = do
  let req = setRequestBodyJSON [[s]] $ parseRequest_ "PUT http://localhost:8126/v0.3/traces"
  res <- httpBS req
  BS.putStrLn $ encode s
  case statusCode $ getResponseStatus res of
    200  -> return ()
    _ -> do
      print $ encode [[s]]
      print req
      putStrLn $ "Unexpected response on put span"++ show res
  return ()

type Name = String
type Resource = String
type Service = String
type SpanId = Integer

defSpan :: Name -> Resource -> Service -> TimeSpec -> IO Span
defSpan name_ resource_ service_ startTime = do
  seed <- randomRIO (1, 100) :: IO Integer
  let spanId = (toNanoSecs startTime * 100+seed) `div` 1000000
  endTime <- getTime Realtime
  return $ Span{
  duration = toNanoSecs $ diffTimeSpec startTime endTime,
  name = name_,
  resource = resource_,
  service = service_,
  span_id = spanId,
  start = toNanoSecs startTime,
  trace_id = spanId}
  
report :: IO a -> Name -> Resource -> Service -> IO a
report action name_ resource_ service_= do
  startTime <- getTime Realtime
  r <- action
  span_ <- defSpan name_ resource_ service_ startTime
  publish span_
  return r
