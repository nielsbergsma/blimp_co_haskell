module Pages.FlightScheduling
  ( Model (..)
  , Action (..)
  , initModel
  , updateModel
  , viewModel
  ) where

import Prelude hiding (show)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Text (Text, show)
import Miso

import Session (Session(..))
import qualified Components.FlightCalendar as FlightCalendar
import qualified Components.Icon as Icon
import qualified Data.FlightScheduling as FlightSchedulingData
import qualified Extensions.Effect as Effect


data Model
  = Initialising
  | Ready { dashboard :: FlightSchedulingData.Dashboard, calendar :: FlightCalendar.Model }
  | Failed Text
  deriving (Eq)


data Action 
  = FetchedDashboard (Either Text FlightSchedulingData.Dashboard)
  | FlightCalendarAction FlightCalendar.Action
  deriving (Eq)


initModel :: Session -> Effect Action Model
initModel _ = 
  Effect.return Initialising (FetchedDashboard <$> FlightSchedulingData.fetchDashboard)


updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchedDashboard (Left reason)) _ = 
  return (Failed reason)

updateModel (FetchedDashboard (Right dashboard)) _ = 
  FlightCalendar.initModel dashboard & Effect.map (Ready dashboard) FlightCalendarAction

updateModel (FlightCalendarAction action) model@(Ready {..}) = 
  FlightCalendar.updateModel action calendar & Effect.map (setCalendar model) FlightCalendarAction

updateModel _ model =
  return model


viewModel :: Model -> View Action
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

viewModel (Ready dashboard calendar) = 
  div_ [ class_ "flex flex-col text-gray-700" ] 
  [ h2_ [ class_ "text-2xl mb-2" ] 
    [ text "Fleet"
    ]
  , div_ [ class_ "overflow-x-scroll whitespace-nowrap mb-8" ]
    (viewAirship <$> sortOn (\airship -> airship.id) dashboard.airships)
  , h2_ [ class_ "text-2xl mb-2" ]
    [ text "Flights"
    ]
  , div_ [ class_ "mb-8" ]
    [ FlightCalendarAction <$> FlightCalendar.viewModel calendar
    ]
  , h2_ [ class_ "text-2xl mb-2" ]
    [ text "Airfields"
    ]
  , div_ [ class_ "overflow-x-scroll whitespace-nowrap mb-8" ]
    (viewAirfield <$> sortOn (\airfield -> airfield.id) dashboard.airfields)
  ]


viewAirship :: FlightSchedulingData.Airship -> View Action
viewAirship airship =
  div_ [ class_ "bg-gray-50 rounded-md p-4 w-64 inline-block mr-4 mb-4" ]
  [ h3_ [ class_ "text-xl mb-2" ]
    [ text (FlightSchedulingData.formatAirshipId airship.id)
    ]
  , img_ [ class_ "rounded-md h-32 w-full object-cover", src_ (formatAirshipImageUrl airship) ]
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "Name"
    ]
  , text airship.name
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "Model"
    ]
  , text airship.model
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "Registration code"
    ]
  , text (FlightSchedulingData.formatAirshipId airship.id)
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "Number of seats"
    ]
  , text (show airship.numberOfSeats)
  ]


viewAirfield :: FlightSchedulingData.Airfield -> View Action
viewAirfield airfield = 
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
  , text airfield.name
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "ICAO code"
    ]
  , text (FlightSchedulingData.formatAirfieldId airfield.id)
  , div_ [ class_ "text-xs pt-2 text-gray-400" ]
    [ text "Coordinates (DD)"
    ]
  , text (formatLocationCoordinatesDD airfield.location)
  ]

-- helpers
formatAirshipImageUrl :: FlightSchedulingData.Airship -> Text
formatAirshipImageUrl airship = 
  "/img/airships/" <> airship.model <> ".webp"


formatAirfieldMapUrl :: FlightSchedulingData.Airfield -> Text
formatAirfieldMapUrl airfield = 
  "/img/airfields/" <> (FlightSchedulingData.formatAirfieldId $ airfield.id) <> ".webp"


formatLocationAsGoogleMapsUrl :: FlightSchedulingData.GeoHash -> Text
formatLocationAsGoogleMapsUrl hash = 
  "https://www.google.com/maps/search/?api=1&query=" <> show latitude <> "," <> show longitude
    where 
      (latitude, longitude) = FlightSchedulingData.geoHashToLatLng hash


formatLocationCoordinatesDD :: FlightSchedulingData.GeoHash -> Text
formatLocationCoordinatesDD hash = 
  formattedLatitude <> ", " <> formattedLongitude
    where 
      formattedLatitude = 
        if latitude < 0
          then (format2f -latitude) <> "° S"
          else (format2f latitude) <> "° N"

      formattedLongitude = 
        if longitude < 0
          then (format2f -longitude) <> "° W"
          else (format2f longitude) <> "° E"

      (latitude, longitude) = 
        FlightSchedulingData.geoHashToLatLng hash

      format2f x = 
        show $ fromIntegral (truncate (x * 100)) / 100


setCalendar :: Model -> FlightCalendar.Model -> Model
setCalendar (model@Ready {}) calendar = model { calendar = calendar }
setCalendar model _ = model
