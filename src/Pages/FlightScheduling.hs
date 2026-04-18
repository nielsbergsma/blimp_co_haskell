module Pages.FlightScheduling
  ( flightScheduling
  ) where

import Miso
import Miso.Html.Element (div_, img_, a_, h2_, h3_)
import Miso.Html.Property (class_, target_, href_, src_)
import Components.Icon qualified as Icon
import Components.FlightCalendar (flightCalendar)

import Session (Session(..))
import Data.List (sortOn)
import Data.FlightScheduling qualified as FlightSchedulingData


data Model
  = Initialising
  | Ready { dashboard :: FlightSchedulingData.Dashboard }
  | Failed MisoString
  deriving (Eq)


data Action 
  = FetchDashboard
  | FetchedDashboard (Either MisoString FlightSchedulingData.Dashboard)
  deriving (Eq)


flightScheduling :: Session -> Component parent Model Action
flightScheduling session = 
  (component (initModel session) updateModel viewModel)
    { mount = Just FetchDashboard }


initModel :: Session -> Model
initModel _ = 
  Initialising


updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchDashboard ->
    FlightSchedulingData.fetchDashboard FetchedDashboard

  FetchedDashboard (Left reason) -> do
    io_ (consoleLog reason)
    modify $ const (Failed reason)

  FetchedDashboard (Right dashboard) ->
    modify $ const (Ready { dashboard = dashboard })


viewModel :: Model -> View Model Action
viewModel Initialising = 
  div_ [ class_ "flex justify-center items-center" ] 
  [ div_ [ class_ "bg-gray-800 text-white w-96 p-4 -mt-6 rounded-b-md text-center" ] 
    [ Icon.spinner [ class_ "w-4 h-4 mr-2" ]
    , text "Fetching dashboard data"
    ]
  ]

viewModel (Failed _) = 
  div_ [ class_ "flex justify-center items-center" ] 
  [ div_ [ class_ "bg-gray-800 text-gray-600 text-white w-96 p-4 -mt-6 rounded-b-md text-center" ] 
    [ Icon.heartCrack [ class_ "w-4 h-4 mr-2" ]
    , text "Failed fetching dashboard data"
    ]
  ]

viewModel (Ready dashboard) = 
  div_ [ class_ "flex flex-col text-gray-700" ] 
  [ h2_ [ class_ "text-2xl mb-2" ] 
    [ text "Fleet"
    ]
  , div_ [ class_ "overflow-x-scroll whitespace-nowrap mb-8" ]
    ((\airship -> withKey (airshipCardKey airship) (airshipCard airship)) <$> sortOn (\airship -> airship.id) dashboard.airships)
  , h2_ [ class_ "text-2xl mb-2" ]
    [ text "Flights"
    ]
  , div_ [ class_ "mb-8" ]
    [ "flight-calendar" +> flightCalendar dashboard
    ]
  , h2_ [ class_ "text-2xl mb-2" ]
    [ text "Airfields"
    ]
  , div_ [ class_ "overflow-x-scroll whitespace-nowrap mb-8" ]
    ((\airfield -> withKey (airfieldCardKey airfield) (airfieldCard airfield)) <$> sortOn (\airfield -> airfield.id) dashboard.airfields)
  ]

-- airship card
airshipCard :: FlightSchedulingData.Airship -> Component parent FlightSchedulingData.Airship ()
airshipCard model = 
  component model noop viewCard
  where
    viewCard airship = 
      div_ [ class_ "bg-gray-50 rounded-md p-4 w-64 inline-block mr-4 mb-4" ]
      [ h3_ [ class_ "text-xl mb-2" ]
        [ text (FlightSchedulingData.formatAirshipId airship.id)
        ]
      , img_ [ class_ "rounded-md h-32 w-full object-cover", src_ (formatAirshipImageUrl airship) ]
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Name"
        ]
      , text (toMisoString airship.name)
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Model"
        ]
      , text (toMisoString airship.model)
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Registration code"
        ]
      , text (FlightSchedulingData.formatAirshipId airship.id)
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Number of seats"
        ]
      , text (toMisoString airship.numberOfSeats)
      ]

airshipCardKey :: FlightSchedulingData.Airship -> Key
airshipCardKey airship = 
  toKey $ "airship-card-" <> FlightSchedulingData.formatAirshipId airship.id


-- airfield card
airfieldCard :: FlightSchedulingData.Airfield -> Component parent FlightSchedulingData.Airfield ()
airfieldCard model = 
  component model noop viewCard
  where
    viewCard airfield = 
      div_ [ class_ "bg-gray-50 rounded-md p-4 w-64 inline-block mr-4 mb-4" ]
      [ h3_ [ class_ "text-xl mb-2" ]
        [ text (FlightSchedulingData.formatAirfieldId airfield.id)
        ]
      , a_ [ target_ "_blank", href_ (formatLocationAsGoogleMapsUrl airfield.location) ]
        [ img_ [ class_ "rounded-md h-32 w-full object-cover", src_ (formatAirfieldMapUrl airfield) ]
        ]
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Name"
        ]
      , text (toMisoString airfield.name)
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "ICAO code"
        ]
      , text (FlightSchedulingData.formatAirfieldId airfield.id)
      , div_ [ class_ "text-xs pt-2 text-gray-400" ]
        [ text "Coordinates (DD)"
        ]
      , text (formatLocationCoordinatesDD airfield.location)
      ]

airfieldCardKey :: FlightSchedulingData.Airfield -> Key
airfieldCardKey airfield = 
  toKey $ "airfield-card-" <> FlightSchedulingData.formatAirfieldId airfield.id

-- helpers
formatAirshipImageUrl :: FlightSchedulingData.Airship -> MisoString
formatAirshipImageUrl airship = 
  "/img/airships/" <> (toMisoString airship.model) <> ".webp"


formatAirfieldMapUrl :: FlightSchedulingData.Airfield -> MisoString
formatAirfieldMapUrl airfield = 
  "/img/airfields/" <> (FlightSchedulingData.formatAirfieldId $ airfield.id) <> ".webp"


formatLocationAsGoogleMapsUrl :: FlightSchedulingData.GeoHash -> MisoString
formatLocationAsGoogleMapsUrl hash = 
  "https://www.google.com/maps/search/?api=1&query=" <> toMisoString latitude <> "," <> toMisoString longitude
    where 
      (latitude, longitude) = FlightSchedulingData.geoHashToLatLng hash


formatLocationCoordinatesDD :: FlightSchedulingData.GeoHash -> MisoString
formatLocationCoordinatesDD hash = 
  formattedLatitude <> ", " <> formattedLongitude
    where 
      formattedLatitude = 
        if latitude < 0
          then format2f (-latitude) <> "° S"
          else format2f latitude <> "° N"

      formattedLongitude = 
        if longitude < 0
          then format2f (-longitude) <> "° W"
          else format2f longitude <> "° E"

      (latitude, longitude) = 
        FlightSchedulingData.geoHashToLatLng hash

      format2f x = 
        toMisoString $ fromIntegral (truncate (x * 100)) / 100

withKey key component_ = toMisoString key +> component_