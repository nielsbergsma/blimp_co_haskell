module Components.FlightCalendar
  ( Model (..)
  , Action
  , initModel
  , updateModel
  , viewModel
  ) where

import Data.List (nub, sort, sortOn)
import Data.Text (Text, pack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day, DayOfWeek(..), dayOfWeek, toGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, TimeOfDay, zonedTimeToUTC)
import qualified Data.FlightScheduling as FlightSchedulingData
import Miso

import qualified Components.Icon as Icon
import Components.Popover (PopoverOrientation(..), popover)
import qualified Extensions.Effect as Effect
import Extensions.Time


data Model = Model 
  { dashboard :: FlightSchedulingData.Dashboard
  , month :: YearMonth
  , time :: UTCTime
  , filteredRoutes :: Set FlightSchedulingData.FlightRoute
  , filteredAirships :: Set FlightSchedulingData.Airship
  , flightPopover :: Maybe (FlightSchedulingData.Flight, Day)
  }
  deriving (Eq)


data Action 
  = SetTime UTCTime
  | SetMonth YearMonth
  | AddRouteFilter FlightSchedulingData.FlightRoute
  | RemoveRouteFilter FlightSchedulingData.FlightRoute
  | AddAirshipFilter FlightSchedulingData.Airship
  | RemoveAirshipFilter FlightSchedulingData.Airship
  | OpenFlightPopover (FlightSchedulingData.Flight, Day)
  | CloseFlightPopover
  deriving (Eq)


initModel :: FlightSchedulingData.Dashboard -> Effect Action Model
initModel dashboard = 
  Effect.return
    (Model dashboard (yearMonthFromUTCTime defaultUTCTime) defaultUTCTime mempty mempty Nothing)
    (SetTime <$> currentUTCTime)


updateModel :: Action -> Model -> Effect Action Model
updateModel (SetTime time) model@(Model {}) = 
  return (model { time = time, month = yearMonthFromUTCTime time })

updateModel (SetMonth month) model@(Model {}) = 
  return (model { month = month, flightPopover = Nothing })

updateModel (AddRouteFilter route) model@(Model {..}) = 
  return (model { filteredRoutes = Set.insert route filteredRoutes, flightPopover = Nothing })

updateModel (RemoveRouteFilter route) model@(Model {..}) = 
  return (model { filteredRoutes = Set.delete route filteredRoutes })

updateModel (AddAirshipFilter airship) model@(Model {..}) = 
  return (model { filteredAirships = Set.insert airship filteredAirships, flightPopover = Nothing })

updateModel (RemoveAirshipFilter airship) model@(Model {..}) = 
  return (model { filteredAirships = Set.delete airship filteredAirships })

updateModel (OpenFlightPopover flightOnDay) model@(Model {}) = 
  return (model { flightPopover = Just flightOnDay })

updateModel (CloseFlightPopover) model@(Model {}) = 
  return (model { flightPopover = Nothing })



viewModel :: Model -> View Action
viewModel (Model {..}) = 
  div_ []
  [ viewRouteFilters (flightRoutes dashboard.flights) filteredRoutes
  , viewAirshipFilters dashboard.airships filteredAirships
  , viewHeader month
  , div_ [ class_ "grid grid-cols-7 gap-4 mb-2" ]
    ((\day -> viewDay day (flightOccurred time) (flightsOfDay filteredRoutes filteredAirships day dashboard.flights)) <$> daysOfMonth month)
  , div_ [ class_ "text-sm text-gray-400" ]
    [ Icon.circleInfo [ class_ "w-4 h-4" ]
    , text " flights are displayed in their local departure and arrival time"
    ]
  , case flightPopover of
      Just (flight, day) -> viewFlightPopover day (flightOccurred time) flight
      Nothing -> noHtml
  ]


viewHeader :: YearMonth -> View Action
viewHeader month = 
  div_ [ class_ "mt-8" ]
  [ h3_ [ class_ "text-xl flex gap-2" ] 
    [ div_ [ class_ "w-40 mb-4" ]
      [ text (formatYearMonth month)
      ]
    , button_
      [ class_ "flex justify-center items-center w-8 h-8 text-sm rounded-full bg-gray-100 hover:bg-gray-800 hover:text-white" 
      , onClick (SetMonth (previousYearMonth month))
      ]
      [ Icon.chevronLeft [ class_ "w-3 h-3 -mt-[0.5em]" ]
      ]
    , button_ 
      [ class_ "flex justify-center items-center w-8 h-8 text-sm rounded-full bg-gray-100 hover:bg-gray-800 hover:text-white" 
      , onClick (SetMonth (nextYearMonth month))
      ]
      [ Icon.chevronRight [ class_ "w-3 h-3 -mt-[0.5em]" ]
      ]
    ]
  ]


viewRouteFilters :: [FlightSchedulingData.FlightRoute] -> Set FlightSchedulingData.FlightRoute -> View Action
viewRouteFilters routes filters = 
  div_ [ class_ "text-sm my-2" ]
  ((\route -> viewRouteFilter route (Set.member route filters)) <$> sort routes)


viewRouteFilter :: FlightSchedulingData.FlightRoute -> Bool -> View Action
viewRouteFilter route filtered = 
  if not filtered 
    then
      button_ 
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-100 hover:bg-gray-800 hover:text-white"
      , onClick (AddRouteFilter route) 
      , title_ (FlightSchedulingData.formatFlightRoute route)
      ]
      [ text (FlightSchedulingData.formatFlightRoute route)
      ]
    else
      button_ 
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-800 text-white hover:bg-gray-600"
      , onClick (RemoveRouteFilter route)
      , title_ (FlightSchedulingData.formatFlightRoute route)
      ]
      [ Icon.filter [ class_ "w-4 h-4 mr-2" ]
      , text (FlightSchedulingData.formatFlightRoute route)
      ]


viewAirshipFilters :: [FlightSchedulingData.Airship] -> Set FlightSchedulingData.Airship -> View Action
viewAirshipFilters airships filters = 
  div_ [ class_ "text-sm my-2" ]
  ((\airship -> viewAirshipFilter airship (Set.member airship filters)) <$> sortOn (\airship -> airship.id) airships)


viewAirshipFilter :: FlightSchedulingData.Airship -> Bool -> View Action
viewAirshipFilter airship filtered = 
  if not filtered 
    then
      button_
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-100 hover:bg-gray-800 hover:text-white"
      , onClick (AddAirshipFilter airship) 
      , title_ airship.name
      ]
      [ text (FlightSchedulingData.formatAirshipId airship.id)
      ]
    else
      button_ 
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-800 text-white hover:bg-gray-600"
      , onClick (RemoveAirshipFilter airship) 
      , title_ airship.name
      ]
      [ Icon.filter [ class_ "w-4 h-4 mr-2" ]
      , text (FlightSchedulingData.formatAirshipId airship.id)
      ]


viewDay :: Day -> FlightOccurred -> [FlightSchedulingData.Flight] -> View Action
viewDay day occured flights =
  div_ [ class_ ("rounded-md bg-gray-50 min-h-[7rem] p-2 " <> columnOffset) ]
  [ div_ [ class_ "mb-2 text-gray-400 text-xs" ]
    [ text (formatMonthDay day)
    ]
  , div_ []
    ((\flight -> viewFlight day occured flight) <$> (sortOn (timeOfDepartureOrArrivalOnDay day) flights))
  ]
    where 
      columnOffset = 
        if firstDayOfTheMonth day == day 
        then formatDayCollumnOffset (dayOfWeek day) 
        else ""


viewFlight :: Day -> FlightOccurred -> FlightSchedulingData.Flight -> View Action
viewFlight day occurred flight = 
  let 
    background = if occurred flight then "bg-gray-500" else "bg-gray-800"
    elementId = formatFlightPopoverId day flight
  in
    button_ 
    [ class_ (background <> " text-white rounded-md px-2 py-1 mb-1 w-full text-left hover:bg-gray-600")
    , id_ elementId
    , onClick (OpenFlightPopover (flight, day))
    ]
    [ div_ [ class_ "text-sm" ]
      [ text (FlightSchedulingData.formatFlightRoute . FlightSchedulingData.route $ flight)
      ]
    , div_ [ class_ "text-xs" ]
      [ text (formatFlightTime day flight)
      ]
    ]


viewFlightPopover :: Day -> FlightOccurred -> FlightSchedulingData.Flight -> View Action
viewFlightPopover day occurred flight = 
  popover forElementId orientation
  [ div_ [ class_ ("w-96 h-80 -mt-28 text-white p-4 rounded-md shadow-lg" <> orientationClass <> backgroundClass) ]
    [ h2_ [ class_ "text-2xl mb-2 flex justify-between" ]
      [ text (FlightSchedulingData.formatFlightRoute (FlightSchedulingData.route flight))
      , button_ [ class_ "rounded-full hover:bg-gray-600 w-8 h-8 flex justify-center items-center", onClick CloseFlightPopover ]
        [ Icon.xmark [ class_ "h-4 w-4 -mt-1" ]
        ]
      ]

    -- departure
    , div_ [ class_ "text-xs pt-4 text-gray-400" ]
      [ text "Departure"
      ]
    , div_ []
      [ text (formatLocalDayAndTime flight.departure.time)
      ]
    , div_ []
      [ text flight.departure.location.name 
      , text (" (" <>  FlightSchedulingData.formatAirfieldId flight.departure.location.id <> ")")
      ]

    -- arrival
    , div_ [ class_ "text-xs pt-4 text-gray-400" ]
      [ text "Arrival"
      ]
    , div_ []
      [ text (formatLocalDayAndTime flight.arrival.time)
      ]
    , div_ [ ]
      [ text flight.arrival.location.name
      , text (" (" <>  FlightSchedulingData.formatAirfieldId flight.arrival.location.id <> ")")
      ]

    -- airship
    , div_ [ class_ "text-xs pt-4 text-gray-400" ]
      [ text "Airship"
      ]
    , div_ []
      [ div_ []
        [ text flight.airship.name
        , text (" (" <> FlightSchedulingData.formatAirshipId flight.airship.id <> ")")
        ]
      , div_ []
        [ text (flight.airship.model)
        ]
      ]
    ]
  ]
    where
      forElementId = formatFlightPopoverId day flight
      orientation = popoverOrientation (dayOfWeek day)
      orientationClass = 
        case orientation of
          OrientateLeft -> " -ml-96 " 
          OrientateRight -> ""
      backgroundClass = 
        if occurred flight
        then " bg-gray-500 "
        else " bg-gray-800 "

-- helpers
flightsOfDay ::  Set FlightSchedulingData.FlightRoute -> Set FlightSchedulingData.Airship -> Day -> [FlightSchedulingData.Flight] -> [FlightSchedulingData.Flight]
flightsOfDay filteredRoutes filteredAirships day flights = 
  filter (\flight -> (departsOnDay flight || arrivesOnDay flight) && matchesRoute flight && matchesAirship flight) flights 
    where 
      matchesRoute flight = Set.null filteredRoutes || Set.member (FlightSchedulingData.route flight) filteredRoutes
      matchesAirship flight = Set.null filteredAirships || Set.member flight.airship filteredAirships
      departsOnDay flight = dayOfZonedTime flight.departure.time == day
      arrivesOnDay flight = dayOfZonedTime flight.arrival.time == day


flightRoutes :: [FlightSchedulingData.Flight] -> [FlightSchedulingData.FlightRoute]
flightRoutes flights = nub (FlightSchedulingData.route <$> flights)


timeOfDepartureOrArrivalOnDay :: Day -> FlightSchedulingData.Flight -> TimeOfDay
timeOfDepartureOrArrivalOnDay day flight = 
  if dayOfZonedTime flight.departure.time == day
  then timeOfZonedTime flight.departure.time
  else timeOfZonedTime flight.arrival.time


formatYearMonth :: YearMonth -> Text
formatYearMonth (YearMonth year month) = 
  (formatLongMonth month) <> " " <> (showText year)


formatMonthDay :: Day -> Text
formatMonthDay day = 
  (formatShortWeekday weekday) <> " " <> (showText dayOfMonth)
    where
      weekday = dayOfWeek day
      (_, _, dayOfMonth) = toGregorian day


formatDayCollumnOffset :: DayOfWeek -> Text
formatDayCollumnOffset Monday = "col-start-1"
formatDayCollumnOffset Tuesday = "col-start-2"
formatDayCollumnOffset Wednesday = "col-start-3"
formatDayCollumnOffset Thursday = "col-start-4"
formatDayCollumnOffset Friday = "col-start-5"
formatDayCollumnOffset Saturday = "col-start-6"
formatDayCollumnOffset Sunday = "col-start-7"


formatFlightTime :: Day -> FlightSchedulingData.Flight -> Text
formatFlightTime day flight = 
  formatTime flight.departure.time <> " - " <> formatTime flight.arrival.time
    where
      formatTime time = 
        if dayOfZonedTime time == day
        then formatLocalTime time 
        else "..."


formatLocalTime :: ZonedTime -> Text 
formatLocalTime = 
  pack <$> formatTime defaultTimeLocale "%H:%M"


formatLocalDayAndTime :: ZonedTime -> Text 
formatLocalDayAndTime = 
  pack <$> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M%Ez"


popoverOrientation :: DayOfWeek -> PopoverOrientation
popoverOrientation Monday = OrientateRight
popoverOrientation Tuesday = OrientateRight
popoverOrientation Wednesday = OrientateRight
popoverOrientation Thursday = OrientateRight
popoverOrientation _ = OrientateLeft


type FlightOccurred = FlightSchedulingData.Flight -> Bool 

flightOccurred :: UTCTime -> FlightOccurred
flightOccurred time flight = 
  time > zonedTimeToUTC flight.arrival.time


formatFlightPopoverId :: Day -> FlightSchedulingData.Flight -> Text
formatFlightPopoverId day flight = 
  FlightSchedulingData.formatFlightId flight.id <> "/" <> showText dayOfMonth
    where
      (_, _, dayOfMonth) = toGregorian day


showText :: Show a => a -> Text
showText = pack . show


noHtml :: View Action 
noHtml = text ""