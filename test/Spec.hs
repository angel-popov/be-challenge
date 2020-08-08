{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators, RecordWildCards #-}

module Main (main) where

import Lib
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher
import qualified DB as DB
import qualified Rentals as R
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           GHC.Generics
import           Control.Exception
import           Servant
import           Servant.Client
import           Servant.Server
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types

main :: IO ()
main = do
  hspec spec

withTestApp :: (Warp.Port -> IO ()) -> IO ()
withTestApp action = do
    -- testWithApplication makes sure the action is executed after the server has
    -- started and is being properly shutdown.
    conn <- DB.initConnectionPool ""
    Warp.testWithApplication (pure $ testApp conn) action

rentalByPriceSpec :: Spec
rentalByPriceSpec =
    -- `around` will start our Server before the tests and turn it off after
    around withTestApp $ do
      -- create a test client function
      let rental :<|> rentals :<|> crash = client (Proxy :: Proxy API)
      let rentalByPrice = \from to -> rentals from to Nothing Nothing Nothing Nothing Nothing
      -- create a servant-client ClientEnv
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

      -- testing scenarios start here
      describe "Filter by price" $ do
        it "should give no users in low price interval" $ \port -> do
          result <- runClientM (rentalByPrice (Just 1) (Just 3)) (clientEnv port)
          result `shouldBe` (Right [])
        it "should give one user in interval 1-3000" $ \port -> do
          result <- runClientM (rentalByPrice (Just 1) (Just 3000)) (clientEnv port)
          length <$> result `shouldBe` (Right 1)
        it "should give 30 users in max interval" $ \port -> do
          result <- runClientM (rentalByPrice (Just 1) (Just 300000)) (clientEnv port)
          length <$> result `shouldBe` (Right 30)
          
rentalByPageSpec :: Spec
rentalByPageSpec =
    -- `around` will start our Server before the tests and turn it off after
    around withTestApp $ do
      -- create a test client function
      let rental :<|> rentals :<|> crash = client (Proxy :: Proxy API)
      let rentalByPage = \offset limit -> rentals Nothing Nothing  offset limit Nothing Nothing Nothing
      -- create a servant-client ClientEnv
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

      describe "Filter by page" $ do
        it "should give one user with offset = 1 limit=1" $ \port -> do
          result <- runClientM (rentalByPage (Just 1) (Just 1)) (clientEnv port)
          length <$> result `shouldBe` (Right 1)
      describe "Filter by page" $ do
        it "should give 13 users with limit 13" $ \port -> do
          result <- runClientM (rentalByPage (Just 1) (Just 13)) (clientEnv port)
          length <$> result `shouldBe` (Right 13)
      describe "Filter by page" $ do
        it "should give 10 users with limit 13 and offset 20" $ \port -> do
          result <- runClientM (rentalByPage (Just 20) (Just 13)) (clientEnv port)
          length <$> result `shouldBe` (Right 10)
      describe "Filter by page" $ do
        it "should give 29 users in with limit 100 and offset 1" $ \port -> do
          result <- runClientM (rentalByPage (Just 1) (Just 100)) (clientEnv port)
          length <$> result `shouldBe` (Right 29)
rentalSorted :: Spec
rentalSorted =
    -- `around` will start our Server before the tests and turn it off after
    around withTestApp $ do
      -- create a test client function
      let rental :<|> rentals :<|> crash = client (Proxy :: Proxy API)
      --                                          pricemin pricemax offset limit    sort  ids     near
      let rentalSort = \price ids -> rentals Nothing  Nothing  Nothing Nothing price ids Nothing
      -- create a servant-client ClientEnv
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

      describe "Sort" $ do
        it "should give all items sorted by price" $ \port -> do
          result <- runClientM (rentalSort (Just "price") Nothing) (clientEnv port)
          length <$> result `shouldBe` (Right 30)
          (((\R.Rental{..}->(_id,_price_per_day)) <$>).take 1) <$> result `shouldBe` (Right [(108507,3000)])
        it "should give selected items sorted by price" $ \port -> do
          result <- runClientM (rentalSort (Just "price") (Just $ RentIds [162781,52210,115462])) (clientEnv port)
          length <$> result `shouldBe` (Right 3)
          (((\R.Rental{..}->(_id,_price_per_day)) <$>).take 1) <$> result `shouldBe` (Right [(52210,15000)])

          -- result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
        -- it "will it fail with a too-small ID?" $ \port -> do
        --   result <- runClientM (createUser 4999) (clientEnv port)
        --   -- result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
rentalNearby :: Spec
rentalNearby =
    -- `around` will start our Server before the tests and turn it off after
    around withTestApp $ do
      -- create a test client function
      let rental :<|> rentals :<|> crash = client (Proxy :: Proxy API)
      --                                  pricemin pricemax offset limit    sort    ids     near
      let rentalNear = \coords -> rentals Nothing  Nothing  Nothing Nothing Nothing Nothing coords
      -- create a servant-client ClientEnv
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

      describe "Nearby" $ do
        it "should give all nearby items" $ \port -> do
          result <- runClientM (rentalNear (Just $ Coords (-117.93) 33.64)) (clientEnv port)
          length <$> result `shouldBe` (Right 6)
          (((\R.Rental{..}->(_id,_price_per_day)) <$>).take 1) <$> result `shouldBe` (Right [(4447,16900)])
        it "should give all nearby items" $ \port -> do
          result <- runClientM (rentalNear (Just $ Coords 33.64 (-117.93) )) (clientEnv port)
          length <$> result `shouldBe` (Right 0)

rentalCamper :: Spec
rentalCamper =
    -- `around` will start our Server before the tests and turn it off after
    around withTestApp $ do
      -- create a test client function
      let rental :<|> rentals :<|> crash = client (Proxy :: Proxy API)
      -- create a servant-client ClientEnv
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

      describe "Rental" $ do
        it "should give rental by id" $ \port -> do
          result <- runClientM (rental 4447) (clientEnv port)
          (R._id . R.item)<$>result `shouldBe` (Right 4447)

spec :: Spec
spec = do
  rentalCamper
  rentalNearby
  rentalSorted
  rentalByPageSpec
  rentalByPriceSpec


            
-- `campervans?price[min]=9000&price[max]=75000`
-- `campervans?page[limit]=3&page[offset]=6`
-- `campervans?ids=2000,51155,54318`
-- `campervans?near=33.64,-117.93` // within 100 miles
-- `campervans?sort=price`
