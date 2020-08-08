{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings       #-}
module Rentals where
import Data.Text
import Data.Aeson
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)
import Data.Time.Clock
import Data.Time.Calendar

data RentalInfo = RentalInfo{
  item :: Rental,
  picts :: [Text]
  }deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Rental = Rental{
  _id :: Integer,
  _name :: Text,
  _type :: Text,
  _description :: Text,
  _sleeps :: Integer,
  _price_per_day :: Integer,
  _home_city :: Text,
  _home_state :: Text,
  _home_zip :: Text,
  _home_county :: Text,
  _home_country :: Text,
  _vehicle_make :: Text,
  _vehicle_model :: Text,
  _vehicle_year :: Integer,
  _vehicle_length :: Rational,
  _created :: UTCTime,
  _updated :: UTCTime,
  _lat :: Double,
  _lng :: Double,
  _primary_image_url :: Text,
  _owner_name :: Text,
  _owner_avatar_url :: Text
  } deriving (Eq, Generic, Show, FromJSON, ToJSON, FromRow)

defRental :: Rental
defRental = Rental{
  _id = 1,
  _name = "name",
  _type = "type",
  _description = "description",
  _sleeps = 1,
  _price_per_day = 2,
  _home_city = "_home_city",
  _home_state = "_home_state",
  _home_zip = "_home_zip",
  _home_county = "_home_county",
  _home_country = "_home_country",
  _vehicle_make = "_vehicle_make",
  _vehicle_model = "_vehicle_model",
  _vehicle_year = 2019,
  _vehicle_length = 2.3,
  _created = UTCTime (ModifiedJulianDay 1000) 10,
  _updated = UTCTime (ModifiedJulianDay 1000) 10,
  _lat = 10.10,
  _lng = 20.20,
  _primary_image_url = "_primary_image_url",
  _owner_name = "_owner_name",
  _owner_avatar_url = "_owner_avatar_url"
  }
