{-# OPTIONS -fno-warn-orphans #-}

module Data.FlightScheduling 
  ( Dashboard(..)
  , Airship(..)
  , Airfield(..)
  , Flight(..)
  , FlightDeparture(..)
  , FlightArrival(..)
  , FlightRoute(..)
  , GeoHash
  , fetchDashboard
  , route
  , geoHashToLatLng
  , formatAirshipId
  , formatAirfieldId
  , formatFlightId
  , formatFlightRoute
  ) where

import Prelude hiding (id)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (find)
import Data.Geohash qualified as Geohash
import Data.Text (Text, unpack)
import Data.Time.LocalTime (ZonedTime(..))
import Data.Time (parseTimeM, defaultTimeLocale)

import Configuration (backendUrl)
import Miso (Effect, MisoString, toMisoString, fromMisoString)
import Miso.JSON
import Miso.Fetch (getJSON, Response(..))


-- model
data Dashboard 
  = Dashboard { airfields :: [Airfield], airships :: [Airship] , flights :: [Flight] }
  deriving (Eq)


newtype FlightId 
  = FlightId Text
  deriving (Eq)


data Flight 
  = Flight { id :: FlightId, departure :: FlightDeparture, arrival :: FlightArrival, airship :: Airship } 
  deriving (Eq)


data FlightDeparture
  = FlightDeparture { time :: ZonedTime, location :: Airfield }
  deriving (Eq)


data FlightArrival 
  = FlightArrival { time :: ZonedTime, location :: Airfield }
  deriving (Eq)


newtype AirfieldId 
  = AirfieldId Text
  deriving (Ord, Eq)


data Airfield 
  = Airfield { id :: AirfieldId, name :: Text, location :: GeoHash, timeZone :: TimeZone }
  deriving (Eq)


newtype AirshipId 
  = AirshipId Text
  deriving (Ord, Eq)


data Airship 
  = Airship { id :: AirshipId, name :: Text, model :: Text, numberOfSeats :: Int }
  deriving (Ord, Eq)


newtype FlightRoute 
  = FlightRoute (Airfield, Airfield)
  deriving (Eq)


instance Ord FlightRoute where
  compare (FlightRoute (ld, la)) (FlightRoute (rd, ra))
    | (ld.id == rd.id) = compare la.id ra.id
    | otherwise = compare ld.id rd.id


newtype TimeZone 
  = TimeZone Text
  deriving (Eq)


newtype GeoHash 
  = GeoHash (Float, Float)
  deriving (Eq)

-- helpers
route :: Flight -> FlightRoute
route (Flight {..}) = FlightRoute (departure.location, arrival.location)


geoHashToLatLng :: GeoHash -> (Float, Float)
geoHashToLatLng (GeoHash coordinate) = coordinate


formatAirshipId :: AirshipId -> MisoString
formatAirshipId (AirshipId id) = toMisoString id


formatAirfieldId :: AirfieldId -> MisoString
formatAirfieldId (AirfieldId id) = toMisoString id


formatFlightId :: FlightId -> MisoString
formatFlightId (FlightId id) = toMisoString id


formatFlightRoute :: FlightRoute -> MisoString
formatFlightRoute (FlightRoute (departure, arrival)) = 
  toMisoString (formatAirfieldId departure.id) <> "-" <> toMisoString (formatAirfieldId arrival.id)


instance Eq ZonedTime where
  (ZonedTime lt1 tz1) == (ZonedTime lt2 tz2) = lt1 == lt2 && tz1 == tz2


fetchDashboard :: (Either MisoString Dashboard -> action) -> Effect parent props model action
fetchDashboard toAction = do
  getJSON (backendUrl <> "/dashboard") [] (toAction . Right . body) (toAction . Left . body)

-- decoders
instance FromJSON Dashboard where
  parseJSON = dashboardParser


dashboardParser :: Value -> Parser Dashboard
dashboardParser (Object value) = do
  airfields <- value .: "airfields" 
  airships <- value .: "airships"
  flights <- value .: "flights" >>= flightsParser airships airfields
  return (Dashboard airfields airships flights)

dashboardParser value = 
  typeMismatch "expected object" value


flightsParser :: [Airship] -> [Airfield] -> Value -> Parser [Flight]
flightsParser airships airfields (Array value) = 
  toList <$> mapM (flightParser airships airfields) value

flightsParser _ _ value = 
  typeMismatch "expected list" value


flightParser :: [Airship] -> [Airfield] -> Value -> Parser Flight
flightParser airships airfields (Object value) = do
  id <- value .: "id"
  departure <- value .: "departure" >>= flightDepartureParser airfields 
  arrival <- value .: "arrival" >>= flightArrivalParser airfields
  airship <- value .: "airship" >>= lookupAirshipParser airships
  return (Flight id departure arrival airship)

flightParser _ _ value = 
  typeMismatch "expected object" value


instance FromJSON FlightId where
  parseJSON (String value) = 
    return (FlightId (fromMisoString value))

  parseJSON value = 
    typeMismatch "expected string" value


instance FromJSON ZonedTime where
  parseJSON (String value) = do
    tryParse "%Y-%m-%dT%H:%M:%S%Ez" stringValue <|> tryParse "%Y-%m-%dT%H:%M:%SZ" stringValue
    where
      stringValue = fromMisoString value
      tryParse format = parseTimeM False defaultTimeLocale format

  parseJSON value = 
    typeMismatch "expected string" value


flightArrivalParser :: [Airfield] -> Value -> Parser FlightArrival
flightArrivalParser airfields (Object value) = do
  time <- value .: "time" 
  location <- value .: "location" >>= lookupAirfieldParser airfields
  return (FlightArrival time location)

flightArrivalParser _ value = 
  typeMismatch "expected object" value


flightDepartureParser :: [Airfield] -> Value -> Parser FlightDeparture
flightDepartureParser airfields (Object value) = do
  time <- value .: "time" 
  location <- value .: "location" >>= lookupAirfieldParser airfields
  return (FlightDeparture time location)

flightDepartureParser _ value = 
  typeMismatch "expected object" value


instance FromJSON AirfieldId where
  parseJSON (String value) = 
    return (AirfieldId (fromMisoString value))

  parseJSON value = 
    typeMismatch "expected string" value


instance FromJSON Airfield where
  parseJSON (Object value) = 
    Airfield
      <$> value .: "id" 
      <*> value .: "name"
      <*> value .: "location"
      <*> value .: "time_zone"

  parseJSON value = 
    typeMismatch "expected object" value


instance FromJSON AirshipId where
  parseJSON (String value) = 
    return (AirshipId (fromMisoString value))
  
  parseJSON value = 
    typeMismatch "expected string" value


instance FromJSON Airship where
  parseJSON (Object value) = 
    Airship
      <$> value .: "id" 
      <*> value .: "name"
      <*> value .: "model"
      <*> value .: "number_of_seats"
  
  parseJSON value = 
    typeMismatch "expected object" value


instance FromJSON TimeZone where
  parseJSON (String value) = 
    return (TimeZone (fromMisoString value))
  
  parseJSON value = 
    typeMismatch "expected string" value


instance FromJSON GeoHash where
  parseJSON (String value) = 
    case Geohash.decode . unpack . fromMisoString $ value of
      Just coordinate -> return (GeoHash coordinate)
      Nothing -> fail "out of bounds"

  parseJSON value = 
    typeMismatch "expected string" value


lookupAirshipParser :: [Airship] -> Value -> Parser Airship
lookupAirshipParser airships value = do
  id <- parseJSON value

  case find (\airship -> airship.id == id) airships of
    Just airship -> return airship
    Nothing -> fail "unknown airship"


lookupAirfieldParser :: [Airfield] -> Value -> Parser Airfield
lookupAirfieldParser airfields value = do
  id <- parseJSON value

  case find (\airfield -> airfield.id == id) airfields of
    Just airfield -> return airfield
    Nothing -> fail "unknown airfield"