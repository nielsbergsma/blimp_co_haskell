module Components.FlightCalendar
  ( flightCalendar
  ) where

import Prelude hiding (show)
import Data.List (nub, sort, sortOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day, DayOfWeek(..), dayOfWeek, toGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, TimeOfDay, zonedTimeToUTC)
import Data.FlightScheduling qualified as FlightSchedulingData

import Miso
import Miso.Html.Element (button_, div_, h2_, h3_)
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_, id_, title_)

import Components.Icon qualified as Icon
import Components.Popover (PopoverOrientation(..), popover)
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


flightCalendar :: FlightSchedulingData.Dashboard -> Component parent props Model Action
flightCalendar dashboard = 
  component (initModel dashboard) updateModel viewModel


initModel :: FlightSchedulingData.Dashboard -> Model
initModel dashboard = 
  (Model dashboard (yearMonthFromUTCTime defaultUTCTime) defaultUTCTime mempty mempty Nothing)

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  (SetTime time) ->
    modify $ \model -> model { time = time, month = yearMonthFromUTCTime time }

  (SetMonth month) ->
   modify $ \model -> (model { month = month, flightPopover = Nothing })

  (AddRouteFilter route) ->
    modify $ \model -> model { filteredRoutes = Set.insert route model.filteredRoutes, flightPopover = Nothing }

  (RemoveRouteFilter route) -> 
    modify $ \model -> model { filteredRoutes = Set.delete route model.filteredRoutes }

  (AddAirshipFilter airship) ->
    modify $ \model -> model { filteredAirships = Set.insert airship model.filteredAirships, flightPopover = Nothing }

  (RemoveAirshipFilter airship) -> 
    modify $ \model -> model { filteredAirships = Set.delete airship model.filteredAirships }

  (OpenFlightPopover flightOnDay) -> 
    modify $ \model -> model { flightPopover = Just flightOnDay }

  (CloseFlightPopover) -> 
    modify $ \model -> model { flightPopover = Nothing }



viewModel :: props -> Model -> View Model Action
viewModel _ (Model {..}) =
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


viewHeader :: YearMonth -> View Model Action
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


viewRouteFilters :: [FlightSchedulingData.FlightRoute] -> Set FlightSchedulingData.FlightRoute -> View Model Action
viewRouteFilters routes filters = 
  div_ [ class_ "text-sm my-2" ]
  ((\route -> viewRouteFilter route (Set.member route filters)) <$> sort routes)


viewRouteFilter :: FlightSchedulingData.FlightRoute -> Bool -> View Model Action
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


viewAirshipFilters :: [FlightSchedulingData.Airship] -> Set FlightSchedulingData.Airship -> View Model Action
viewAirshipFilters airships filters = 
  div_ [ class_ "text-sm my-2" ]
  ((\airship -> viewAirshipFilter airship (Set.member airship filters)) <$> sortOn (\airship -> airship.id) airships)


viewAirshipFilter :: FlightSchedulingData.Airship -> Bool -> View Model Action
viewAirshipFilter airship filtered = 
  if not filtered 
    then
      button_
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-100 hover:bg-gray-800 hover:text-white"
      , onClick (AddAirshipFilter airship) 
      , title_ (toMisoString airship.name)
      ]
      [ text (FlightSchedulingData.formatAirshipId airship.id)
      ]
    else
      button_ 
      [ class_ "rounded-full mr-2 mb-2 w-40 py-1 px-4 bg-gray-800 text-white hover:bg-gray-600"
      , onClick (RemoveAirshipFilter airship) 
      , title_ (toMisoString airship.name)
      ]
      [ Icon.filter [ class_ "w-4 h-4 mr-2" ]
      , text (FlightSchedulingData.formatAirshipId airship.id)
      ]


viewDay :: Day -> FlightOccurred -> [FlightSchedulingData.Flight] -> View Model Action
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


viewFlight :: Day -> FlightOccurred -> FlightSchedulingData.Flight -> View Model Action
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


viewFlightPopover :: Day -> FlightOccurred -> FlightSchedulingData.Flight -> View Model Action
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
      [ text (toMisoString flight.departure.location.name)
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
      [ text (toMisoString flight.arrival.location.name)
      , text (" (" <>  FlightSchedulingData.formatAirfieldId flight.arrival.location.id <> ")")
      ]

    -- airship
    , div_ [ class_ "text-xs pt-4 text-gray-400" ]
      [ text "Airship"
      ]
    , div_ []
      [ div_ []
        [ text (toMisoString flight.airship.name)
        , text (" (" <> FlightSchedulingData.formatAirshipId flight.airship.id <> ")")
        ]
      , div_ []
        [ text (toMisoString flight.airship.model)
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
flightRoutes flights = 
  nub (FlightSchedulingData.route <$> flights)


timeOfDepartureOrArrivalOnDay :: Day -> FlightSchedulingData.Flight -> TimeOfDay
timeOfDepartureOrArrivalOnDay day flight = 
  if dayOfZonedTime flight.departure.time == day
  then timeOfZonedTime flight.departure.time
  else timeOfZonedTime flight.arrival.time


formatYearMonth :: YearMonth -> MisoString
formatYearMonth (YearMonth year month) = 
  (formatLongMonth month) <> " " <> (formatYear year)


formatMonthDay :: Day -> MisoString
formatMonthDay day = 
  (formatShortWeekday weekday) <> " " <> (toMisoString dayOfMonth)
    where
      weekday = dayOfWeek day
      (_, _, dayOfMonth) = toGregorian day


formatDayCollumnOffset :: DayOfWeek -> MisoString
formatDayCollumnOffset Monday = "col-start-1"
formatDayCollumnOffset Tuesday = "col-start-2"
formatDayCollumnOffset Wednesday = "col-start-3"
formatDayCollumnOffset Thursday = "col-start-4"
formatDayCollumnOffset Friday = "col-start-5"
formatDayCollumnOffset Saturday = "col-start-6"
formatDayCollumnOffset Sunday = "col-start-7"


formatFlightTime :: Day -> FlightSchedulingData.Flight -> MisoString
formatFlightTime day flight = 
  format flight.departure.time <> " - " <> format flight.arrival.time
    where
      format time = 
        if dayOfZonedTime time == day
        then formatLocalTime time 
        else "..."


formatLocalTime :: ZonedTime -> MisoString 
formatLocalTime = 
  toMisoString <$> formatTime defaultTimeLocale "%H:%M"


formatLocalDayAndTime :: ZonedTime -> MisoString 
formatLocalDayAndTime = 
  toMisoString <$> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M%Ez"


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


formatFlightPopoverId :: Day -> FlightSchedulingData.Flight -> MisoString
formatFlightPopoverId day flight = 
  FlightSchedulingData.formatFlightId flight.id <> "/" <> toMisoString dayOfMonth
    where
      (_, _, dayOfMonth) = toGregorian day


noHtml :: View Model Action 
noHtml = text ""