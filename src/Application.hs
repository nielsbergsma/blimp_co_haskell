module Application where

import Components.Icon qualified as Icon
import Components.SignInModal (signInModal)
import Components.Header (header, headerKey)
import Pages.NotFound (notFound)
import Pages.FlightScheduling (flightScheduling)
import Pages.Reservations (reservations)

import Miso
import Miso.Html.Element (div_, h1_)
import Miso.Html.Property (class_)

import Routes (Route)
import Routes qualified as Routes
import Session qualified as Session
import Session (Session)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

data Model
  = Initialising { route :: Maybe Routes.Route }
  | NotSignedIn { route :: Maybe Routes.Route }
  | SignedIn { route :: Maybe Routes.Route, page :: Page, session :: Session }
  deriving (Eq)

data Page 
  = NotFound
  | Reservations
  | FlightScheduling
  deriving (Eq)

data Action
  = Initialise
  | RouteChanged (Maybe Route)
  | SignInCompleted Session
  | SignOutCompleted 
  deriving (Eq)

main :: IO ()
main = do
  currentRoute <- Routes.fromCurrentURI
  startApp defaultEvents $ application currentRoute
  

application :: Maybe Route -> App Model Action
application initialRoute =
  (component (initModel initialRoute) updateModel viewModel)
    { subs = 
      [ routerSub (RouteChanged . rightToMaybe)
      , Session.subscribeToSignedIn SignInCompleted 
      , Session.subscribeToSignedOut SignOutCompleted
      ]
    , mount = Just Initialise
    , logLevel = Off
    }


initModel :: Maybe Routes.Route -> Model
initModel route = 
  Initialising { route = route }


updateModel :: Action -> Effect parent Model Action
updateModel = \case
  action@Initialise -> do
    -- route to default route if no valid route is present
    currentRoute <- getRoute <$> get
    case currentRoute of
      Nothing -> do
        modify $ \model -> model { route = Just Routes.defaultRoute } 
        Routes.redirect Routes.defaultRoute
        
      Just _ -> 
        noop action

    Session.restore

  SignInCompleted session -> do 
    modify $ \model -> do 
      let currentRoute = getRoute model
      SignedIn { route = currentRoute, page = pageFromRoute currentRoute, session = session }

  SignOutCompleted ->
    modify $ \model -> NotSignedIn { route = getRoute model }
  
  RouteChanged newRoute ->
    modify (setRoute newRoute)


viewModel :: Model -> View Model Action
viewModel (Initialising {}) =
  div_ [ class_ "h-[100vh] bg-gray-800 animate-loaded" ] 
  [ div_ [ class_ "flex flex-col justify-center items-center text-gray-300" ] 
    [ div_ [ class_ "text-[16rem]"] 
      [ Icon.mugHot [ class_ "w-48 h-48" ]
      ]
    , div_ [ class_ "text-4xl my-2" ] 
      [ text "Taking off, hold tight..."
      ]
    , div_ [ class_ "mb-4" ]
      [ text "Initialising application"
      ]
    ]
  ]

viewModel (NotSignedIn { route }) =
  div_ [ class_ "min-h-full animate-loaded" ] 
  [ div_ [ ] 
    [ div_ [ key_ (headerKey route Nothing) ]
      [ "header" +> header route Nothing
      ]
    , div_ [ class_ "flex flex-col justify-center items-center text-gray-300" ] 
      [ div_ [ class_ "text-[16rem]"] 
        [ Icon.userSlash [ class_ "w-64 h-64" ]
        ]
      , div_ [ class_ "text-4xl mt-2" ] 
        [ text "Not Signed In"
        ]
      ]
    ]
  , div_ [ class_ "modal-container transition-all transition duration-700 ease-in-out" ]
    [ "sign-in-modal" +> signInModal
    ]
  ]

viewModel (SignedIn { route, session, page }) =
  div_ [ class_ "min-h-full animate-loaded" ] 
  [ div_ [ key_ (headerKey route (Just session)) ]
    [ "header" +> header route (Just session)
    ]
  , div_ [ class_ "p-8 -mt-64" ]
    [ h1_ [ class_ "text-3xl text-white"]
      [ text (pageTitle page)
      ]
    , div_ [ class_ "mx-auto pt-8 pb-12" ]
      [ div_ [ class_ "rounded-lg bg-white px-5 py-6 shadow min-h-[50vh]" ]
        [ case page of
            NotFound -> "not-found-page" +> notFound 
            FlightScheduling -> "flight-scheduling-page" +> flightScheduling session
            Reservations -> "reservations-page" +> reservations session
        ]
      ]
    ]
  ]

pageFromRoute :: Maybe Route -> Page
pageFromRoute (Just Routes.FlightScheduling) = FlightScheduling
pageFromRoute (Just Routes.Reservations) = Reservations
pageFromRoute _ = NotFound

pageTitle :: Page -> MisoString
pageTitle NotFound = "Page not found"
pageTitle FlightScheduling = "Flight scheduling"
pageTitle Reservations = "Reservations"

getRoute :: Model -> Maybe Route
getRoute (Initialising {route}) = route
getRoute (NotSignedIn {route}) = route
getRoute (SignedIn {route}) = route

setRoute :: Maybe Route -> Model -> Model
setRoute newRoute model@(Initialising {}) = model { route = newRoute }
setRoute newRoute model@(NotSignedIn {}) = model { route = newRoute }
setRoute newRoute model@(SignedIn {}) = model { route = newRoute, page = pageFromRoute newRoute }

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
