{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings       #-}
module Lib
    ( startApp
    , testApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Rentals
import Data.ByteString.Lazy.UTF8

type API = -- "rentals" :> Get '[JSON] [Rental] :<|>
           "rentals" :> Capture "rentalId" Integer :> Get '[JSON] Rental :<|>
           "rentals" :> QueryParams "price" Integer :> QueryParam "pricemin" Integer :> QueryParam "pricemax" Integer :> Get '[JSON] [Rental]

startApp :: IO ()
startApp = run 8080 testApp

testApp :: Application
testApp = serve api testServer

api :: Proxy API
api = Proxy

testServer :: Server API
testServer = rental :<|>
         rentalByPrice

rentalByPrice :: [Integer] -> Maybe Integer -> Maybe Integer-> Handler [Rental]
rentalByPrice [] Nothing Nothing = return rentals
rentalByPrice (form:[to]) Nothing Nothing = return []
rentalByPrice (form:[]) Nothing Nothing = return []
rentalByPrice [] (Just from) Nothing = return []
rentalByPrice [] Nothing (Just to) = return []
rentalByPrice [] (Just from) (Just to) = return []
rentalByPrice price minPrice maxPrice = throwError err400 {errBody= "Invalid parameters:" <> fromString (show price ++ "" ++ " " ++ show minPrice ++ " " ++ show maxPrice)}

rental :: Integer -> Handler Rental
rental 1 = return defRental
rental n = throwError $ err404 { errBody= "Rental Not Found"}

rentals :: [Rental]
rentals = [ (defRental{_id=2}), defRental ]
