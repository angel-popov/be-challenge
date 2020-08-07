module DB.hs where

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
    createPool (connectPostgreSQL connStr)
                 close
                 2 -- stripes
                 60 -- unused connections are kept open for a minute
                 10 -- max. 10 connections open per stripe


