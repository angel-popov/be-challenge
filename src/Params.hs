{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Params where
import Servant
import Data.String.Interpolate ( i, iii )
import qualified Data.Text as T
import Data.Text.Read
import Text.Read

newtype Long = Long Double deriving Show
newtype Latt = Latt Double deriving Show

newtype Coords = Coords (Long, Latt) deriving Show
instance ToHttpApiData Coords where
  toQueryParam (Coords ((Long lng), (Latt lat))) = [i|#{lng},#{lat}|]
instance FromHttpApiData Coords where
  parseQueryParam param =
    let first = T.takeWhile (/=',') param
        second = T.drop 1 $ T.dropWhile (/=',') param in
    case (,) <$>((Long . fst) <$> (rational first)) <*> ((Latt . fst) <$> (rational second)) of
      (Right r) -> Right $ Coords r
      (Left err) -> (Left $ [i|Failed to parse coords #{param} long:'#{first}' latt:'#{second}' #{err}|])

newtype RentIds = RentIds [Integer] deriving Show
instance ToHttpApiData RentIds where
  toQueryParam (RentIds ids) = T.intercalate "," $ (T.pack . show) <$> ids
instance FromHttpApiData RentIds where
  parseQueryParam param = 
    case RentIds <$> readEither ( "[" ++ T.unpack param ++ "]") of
      (Right r) -> Right r
      (Left err) -> (Left $ [i|Failed to parse ids from #{param} - #{err}|])
      
newtype PriceMin = PriceMin Integer
instance FromHttpApiData PriceMin where
  parseQueryParam param = either (const $ Left [i|"Failed to parse pricemin from ${param}"|]) Right $
    PriceMin <$> readEither (T.unpack param)
instance ToHttpApiData PriceMin where
  toQueryParam (PriceMin price) = [i|#{price}|]

newtype PriceMax = PriceMax Integer
instance FromHttpApiData PriceMax where
  parseQueryParam param = either (const $ Left [i|"Failed to parse pricemax from ${param}"|]) Right $
    PriceMax <$> readEither (T.unpack param)
instance ToHttpApiData PriceMax where
  toQueryParam (PriceMax price) = [i|#{price}|]

newtype PageLimit = PageLimit Integer
instance FromHttpApiData PageLimit where
  parseQueryParam param = either (const $ Left [i|"Failed to parse pagelimit from ${param}"|]) Right $
    PageLimit <$> readEither (T.unpack param)
instance ToHttpApiData PageLimit where
  toQueryParam (PageLimit limit) = [i|#{limit}|]
    
newtype PageOffset = PageOffset Integer
instance FromHttpApiData PageOffset where
  parseQueryParam param = either (const $ Left [i|"Failed to parse pageoffset from ${param}"|]) Right $
    PageOffset <$> readEither (T.unpack param)
instance ToHttpApiData PageOffset where
  toQueryParam (PageOffset offs) = [i|#{offs}|]
