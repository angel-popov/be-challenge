{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DB where
import Rentals
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Data.Pool
import Data.ByteString
import Data.String.Interpolate (i)
import Data.ByteString.UTF8
import Control.Exception
import qualified Data.Text as T

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
    createPool (connectPostgreSQL connStr)
                 close
                 2 -- stripes
                 60 -- unused connections are kept open for a minute
                 10 -- max. 10 connections open per stripe

selectRentals :: Pool Connection -> String -> IO (Either SomeException [Rental])
selectRentals conns selectQuery = do
  try $ withResource conns $ \conn ->
    query_ conn $ Query $ fromString selectQuery

getRental :: Pool Connection -> Integer -> IO (Either SomeException ([Rental],[[T.Text]]))
getRental conns rId = do
  try $ withResource conns $ \conn -> do
    r <- (query_ conn . Query . fromString)[i|select * from rentals where id = #{rId}|]
    u <- (query_ conn . Query . fromString)[i|select url from rental_images where rental_id = #{rId}|]
    return (r,((T.pack<$>) )<$> u)
