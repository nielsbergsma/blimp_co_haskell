{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( Route(..)
  , defaultRoute
  , redirect
  , fromCurrentURI
  , subscribeToChanges
  ) where

import Miso (Decoder(..), DecodeTarget(..), Effect, Sub, windowSub, getURI, io_, pushRoute)
import Miso.Router (Router(..), path, toPath, routeParser, routes, route)
import Miso.JSON

data Route
  = FlightScheduling
  | Reservations
  deriving (Eq)


instance Router Route where
  routeParser = routes 
    [ FlightScheduling <$ path "flight-scheduling"
    , Reservations <$ path "reservations"
    ]

  fromRoute FlightScheduling = [ toPath "flight-scheduling" ]
  fromRoute Reservations = [ toPath "reservations" ]


fromCurrentURI :: IO (Maybe Route)
fromCurrentURI =
  rightToMaybe . route <$> getURI


defaultRoute :: Route
defaultRoute = 
  FlightScheduling


redirect :: Router route => route -> Effect parent props model action
redirect destination = 
  io_ (pushRoute destination)


subscribeToChanges :: (Maybe Route -> action) -> Sub action
subscribeToChanges action = windowSub "routechanged" eventDecoder (action . rightToMaybe . toRoute)
  where
    eventDecoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = withObject "Event" $ \event -> event .: "detail"
      }      
      

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just