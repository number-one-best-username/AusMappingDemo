{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module PackPop where

import Data.Geospatial
import qualified Data.Sequence as S
import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy as BS
import GHC.Generics

data StatLevel = StatLevel
  { info_sa2 :: String
  , info_sa3 :: String
  , info_sa4 :: String
  , info_sa5 :: String
  , info_sa6 :: String
  , info_sa2_id :: String
  , info_state :: String
  , info_area :: Float
  } deriving (Show)
instance FromJSON StatLevel where
  parseJSON = withObject "StatLevel" $ \v -> StatLevel
    <$> v .: "SA2_NAME16"
    <*> v .: "SA3_NAME16"
    <*> v .: "SA4_NAME16"
    <*> v .: "GCC_NAME16"
    <*> v .: "STE_NAME16"
    <*> v .: "SA2_MAIN16"
    <*> v .: "STE_NAME16"
    <*> v .: "AREASQKM16"

data PopProp = PopProp
  { pop_suburb :: String
  , pop_sa2_id :: Int
  , pop_persons :: Int
  , pop_median :: Float
  } deriving (Show)
instance FromJSON PopProp where
  parseJSON = withObject "PopProp" $ \v -> PopProp
    <$> v .: "SA2_NAME_2016"
    <*> v .: "SA2_MAINCODE_2016"
    <*> v .: "Persons"
    <*> v .: "Median_age_persons"

data PackedPop = PackedPop {
  sa2 :: String,
  sa3 :: String,
  sa4 :: String,
  sa5 :: String,
  sa6 :: String,
  population :: Int,
  age :: Float,
  area :: Float
  } deriving (Show, Generic)
instance ToJSON PackedPop where

packPop :: IO (GeoFeatureCollection PackedPop)
packPop = do
  sa2 <- decodeFileStrict "SA2.geojson" >>= return . fromMaybe (GeoFeatureCollection Nothing S.empty)
  sa2pop <- decodeFileStrict "SA2_pop.geojson" >>= return . fromMaybe (GeoFeatureCollection Nothing S.empty)
  let merged = mergeGeoFeatures packPopFunction popMergeFunction sa2 sa2pop
  BS.writeFile "geo.json" $ encode merged
  return merged

popMergeFunction :: S.Seq (GeoFeature StatLevel) -> S.Seq (GeoFeature PopProp) -> S.Seq (GeoFeature StatLevel, GeoFeature PopProp)
popMergeFunction alpha beta = S.zip (S.sortOn sA $ S.filter (fA beta) alpha) (S.sortOn sB $ S.filter (fB alpha) beta)
  where sA (GeoFeature _ _ aProps _) = read $ info_sa2_id aProps :: Int
        sB (GeoFeature _ _ bProps _) = pop_sa2_id bProps
        fA bs (GeoFeature _ _ aProps _) = S.length (S.filter (\(GeoFeature _ _ bProps _) -> info_sa2_id aProps == show (pop_sa2_id bProps)) bs) > 0
        fB as (GeoFeature _ _ bProps _) = S.length (S.filter (\(GeoFeature _ _ aProps _) -> show (pop_sa2_id bProps) == info_sa2_id aProps) as) > 0

packPopFunction :: StatLevel -> PopProp -> PackedPop
packPopFunction info pop = PackedPop
                           (info_sa2 info)
                           (info_sa3 info)
                           (info_sa4 info)
                           (info_sa5 info)
                           (info_sa6 info)
                           (pop_persons pop)
                           (pop_median pop)
                           (info_area info)

morphGeoFeatures :: (old -> new) -> GeoFeatureCollection old -> GeoFeatureCollection new
morphGeoFeatures f (GeoFeatureCollection box geo) = GeoFeatureCollection box $ fmap (geoFeature f) geo
  where geoFeature f (GeoFeature box' geo' props id') = GeoFeature box' geo' (f props) id'

-- Merge two feature collections with first collection overwriting second collection
mergeGeoFeatures :: (alpha -> beta -> merged)
                 -> (S.Seq (GeoFeature alpha) -> S.Seq (GeoFeature beta) -> S.Seq (GeoFeature alpha, GeoFeature beta))
                 -> GeoFeatureCollection alpha -> GeoFeatureCollection beta -> GeoFeatureCollection merged

mergeGeoFeatures f m (GeoFeatureCollection aBox aGeo) (GeoFeatureCollection _ bGeo)  = GeoFeatureCollection aBox $ fmap (geoFeature f) (m aGeo bGeo)
  where geoFeature f ((GeoFeature box' geo' aProps id'), (GeoFeature _ _ bProps _)) = GeoFeature box' geo' (f aProps bProps) id'
