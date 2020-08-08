{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Rentals
import Data.String.Interpolate ( i, iii )

import qualified DB as DB
import Data.Pool
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.UTF8 as C (toString)
import Control.Exception (Exception, SomeException, throw)


import Network.Wai                            (Request, rawPathInfo,
                                               requestHeaderHost)
import Network.Wai.Handler.Warp               (defaultOnException,
                                                defaultSettings,
                                                runSettings,
                                                setOnException,
                                                setPort)
import System.Log.Raven                       (initRaven, register,
                                               silentFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types                 (SentryLevel (Error),
                                               SentryRecord (..))

import qualified Data.Text as T
import Data.Text.Read
import Text.Read
data Coords = Coords Double Double deriving Show
instance ToHttpApiData Coords where
  toQueryParam (Coords lat lng) = [i|#{lat},#{lng}|]
instance FromHttpApiData Coords where
  parseQueryParam param =
    let first = T.takeWhile (/=',') param
        second = T.drop 1 $ T.dropWhile (/=',') param in
    case Coords<$>(fst <$> (rational first)) <*> (fst <$> (rational second)) of
      (Right r) -> Right r
      (Left err) -> (Left $ [i|Failed to parse coords #{param} lat:'#{first}' long:'#{second}' #{err}|])

data RentIds = RentIds [Integer] deriving Show
instance ToHttpApiData RentIds where
  toQueryParam (RentIds ids) = T.intercalate "," $ (T.pack . show) <$> ids
instance FromHttpApiData RentIds where
  parseQueryParam param = 
    case RentIds <$> readEither ( "[" ++ T.unpack param ++ "]") of
      (Right r) -> Right r
      (Left err) -> (Left $ [i|Failed to parse ids from #{param} - #{err}|])

type API = 
           "rentals" :> Capture "rentalId" Integer :> Get '[JSON] RentalInfo :<|>
           "rentals" :> QueryParam "pricemin" Integer :> QueryParam "pricemax" Integer
                     :> QueryParam "pagelimit" Integer :> QueryParam "pageoffset" Integer
                     :> QueryParam "sort" String
                     :> QueryParam "ids" RentIds :> QueryParam "near" Coords
                     :> Get '[JSON] [Rental] :<|>
           "crash"   :> Get '[JSON] String

startApp :: IO ()
startApp =
  let settings =
        setPort 8080 $
        setOnException sentryOnException $
        defaultSettings in do
    c <- DB.initConnectionPool ""
    runSettings settings $ serve (Proxy :: Proxy API) (testServer c)

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




testApp :: Pool Connection -> Application
testApp c = serve api $ testServer c

api :: Proxy API
api = Proxy

testServer :: Pool Connection -> Server API
testServer conns =
  rental conns :<|>
  rentals conns :<|>
  crashMe
data MyException = MyException deriving (Show)
instance Exception MyException

crashMe :: Handler String
crashMe = throw MyException >> return "ok"

composeQuery :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe String -> Maybe RentIds -> Maybe Coords -> String
composeQuery pricemin pricemax offset limit sort ids near =
  let condPart = concat [priceMinMax pricemin pricemax, some ids, nearBy near]
      wherePart = if condPart==[] then "" else ("where " ++ L.intercalate " and " condPart) in
    [iii|select * from rentals #{wherePart}
        #{orderBy sort}
        #{limitOffset limit offset} |]
    where
      priceMinMax :: Maybe Integer -> Maybe Integer -> [String]
      priceMinMax Nothing Nothing = []
      priceMinMax (Just pmin) Nothing = ["price_per_day <= " ++ show pmin]
      priceMinMax Nothing (Just pmax) = ["price_per_day >= " ++ show pmax]
      priceMinMax (Just pmin) (Just pmax) = ["price_per_day BETWEEN " ++ show pmin
                                            ++" and " ++ show pmax]
      some :: Maybe RentIds -> [String]
      some Nothing = []
      some (Just (RentIds ids_)) = ["id in (" ++ L.intercalate "," (show <$> ids_) ++ ")"]
      nearBy :: Maybe Coords -> [String]
      nearBy Nothing = []
      nearBy (Just (Coords lng lat)) = [[i|(point(#{lng}, #{lat}) <@> point(lng, lat)) < 100|]]
      orderBy :: Maybe String -> String
      orderBy (Just "price") = "order by price_per_day"
      orderBy _ = "order by id"
      limitOffset :: Maybe Integer -> Maybe Integer -> String
      limitOffset Nothing Nothing = ""
      limitOffset Nothing (Just offs) =  [i|offset #{offs}|] 
      limitOffset (Just lim) Nothing =   [i|limit #{lim}|]
      limitOffset (Just lim) (Just offs) = [i|offset #{offs} limit #{lim}|]
  
rentals ::  Pool Connection -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe String -> Maybe RentIds -> Maybe Coords -> Handler [Rental]
rentals c pricemin pricemax
       offset limit
       sort
       ids near = do
  let sqlQ = composeQuery pricemin pricemax offset limit sort ids near
  liftIO $ putStrLn sqlQ
  res <-liftIO $ DB.selectRentals c sqlQ
  case res of
    Right filtered -> return filtered
    Left err -> do
      liftIO $ putStrLn sqlQ
      liftIO $ print err
      throwError $ err500 { errBody = fromString $ show err }

rental :: Pool Connection -> Integer -> Handler RentalInfo
rental c n = do
  res <-liftIO $ DB.getRental c n
  case res of
    Right ([rent], urls) -> return $ RentalInfo rent $ concat urls
    Right ([],[]) -> throwError $ err400 {errBody = "RentalId not found"}
    Right ([],(_:_)) -> throwError $ err500 {errBody = "RentalId not found, but there are pictures of it."}
    Right ((_:_:_),_) -> throwError $ err500 {errBody = "More than one item with that rentalId is found."}
    Left err -> do
      throwError $ err500 { errBody = fromString $ show err }
