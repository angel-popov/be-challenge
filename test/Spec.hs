{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (testApp)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return testApp) $ do
    describe "GET /rentals" $ do
        it "responds with 200" $ do
            get "/rentals" `shouldRespondWith` 200
        it "responds with [Rental]" $ do
            let users1 = "[{\"_owner_avatar_url\":\"_owner_avatar_url\",\"_type\":\"type\",\"_created\":1000,\"_vehicle_year\":2019,\"_vehicle_model\":\"_vehicle_model\",\"_vehicle_make\":\"_vehicle_make\",\"_id\":2,\"_home_county\":\"_home_county\",\"_name\":\"name\",\"_description\":\"description\",\"_home_zip\":\"_home_zip\",\"_vehicle_length\":2.3,\"_home_country\":\"_home_country\",\"_primary_image_url\":\"_primary_image_url\",\"_updated\":10000,\"_lng\":20.2,\"_home_city\":\"_home_city\",\"_home_state\":\"_home_state\",\"_sleeps\":1,\"_owner_name\":\"_owner_name\",\"_lat\":10.1,\"_price_per_day\":2}," :: Body
                users2="{\"_owner_avatar_url\":\"_owner_avatar_url\",\"_type\":\"type\",\"_created\":1000,\"_vehicle_year\":2019,\"_vehicle_model\":\"_vehicle_model\",\"_vehicle_make\":\"_vehicle_make\",\"_id\":1,\"_home_county\":\"_home_county\",\"_name\":\"name\",\"_description\":\"description\",\"_home_zip\":\"_home_zip\",\"_vehicle_length\":2.3,\"_home_country\":\"_home_country\",\"_primary_image_url\":\"_primary_image_url\",\"_updated\":10000,\"_lng\":20.2,\"_home_city\":\"_home_city\",\"_home_state\":\"_home_state\",\"_sleeps\":1,\"_owner_name\":\"_owner_name\",\"_lat\":10.1,\"_price_per_day\":2}]" :: Body
            get "/rentals" `shouldRespondWith` 200 {matchBody = bodyEquals (users1<>users2)}
    describe "GET /rentals/1" $ do
        it "responds with 200" $ do
            get "/rentals/1" `shouldRespondWith` 200
        it "responds with rental info" $ do
            let user1="{\"_owner_avatar_url\":\"_owner_avatar_url\",\"_type\":\"type\",\"_created\":1000,\"_vehicle_year\":2019,\"_vehicle_model\":\"_vehicle_model\",\"_vehicle_make\":\"_vehicle_make\",\"_id\":1,\"_home_county\":\"_home_county\",\"_name\":\"name\",\"_description\":\"description\",\"_home_zip\":\"_home_zip\",\"_vehicle_length\":2.3,\"_home_country\":\"_home_country\",\"_primary_image_url\":\"_primary_image_url\",\"_updated\":10000,\"_lng\":20.2,\"_home_city\":\"_home_city\",\"_home_state\":\"_home_state\",\"_sleeps\":1,\"_owner_name\":\"_owner_name\",\"_lat\":10.1,\"_price_per_day\":2}"
            get "/rentals/1" `shouldRespondWith` user1
    describe "GET /rentals/100" $ do
        it "responds with rental info" $ do
            get "/rentals/100" `shouldRespondWith` 404 {matchBody = bodyEquals "Rental Not Found"}
    describe "GET /rentals/100" $ do
        it "responds with rental info" $ do
            get "/rentals/100" `shouldRespondWith` 404 {matchBody = bodyEquals "Rental Not Found"}
    describe "get rentals by price price[min], price[max]=75000" $ do
        it "/rentals?price=9000&price=75000" $ do
            get "/rentals?price=9000&price=75000" `shouldRespondWith` 200 {matchBody = bodyEquals "[]"}
        it "/rentals?price=9000" $ do
            get "/rentals?price=9000" `shouldRespondWith` 200 {matchBody = bodyEquals "[]"}
        it "/rentals?price=9000&pricemax=75000" $ do
            get "/rentals?price=9000&pricemax=75000" `shouldRespondWith` 400 {matchBody = bodyEquals "Invalid parameters:[9000] Nothing Just 75000"}

            
-- `campervans?price[min]=9000&price[max]=75000`
-- `campervans?page[limit]=3&page[offset]=6`
-- `campervans?ids=2000,51155,54318`
-- `campervans?near=33.64,-117.93` // within 100 miles
-- `campervans?sort=price`
