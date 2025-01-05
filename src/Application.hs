module Application (main) where

import Data.Text (Text)
import Miso hiding (route)
import Data.Function ((&))
import qualified Language.Javascript.JSaddle.Wasm as Wasm

import qualified Components.SignInModal as SignInModal
import qualified Components.Icon as Icon
import qualified Components.Header as Header 
import qualified Pages.NotFound as NotFoundPage
import qualified Pages.FlightScheduling as FlightSchedulingPage
import qualified Pages.Reservations as ReservationsPage

import qualified Extensions.Effect as Effect
import qualified Routes
import qualified Session
import Session (Session)


data Model
  = Initialising { route :: Maybe Routes.Route, header :: Header.Model }
  | NotSignedIn { route :: Maybe Routes.Route, header :: Header.Model }
  | SignedIn { route :: Maybe Routes.Route, header :: Header.Model, page :: Page, session :: Session }
  deriving (Eq)


data Page 
  = NotFound NotFoundPage.Model
  | Reservations ReservationsPage.Model
  | FlightScheduling FlightSchedulingPage.Model
  deriving (Eq)


data Action
  = Initialise
  | RouteChanged Text
  | HeaderAction Header.Action
  | SignInModalAction SignInModal.Action
  | NotFoundPageAction NotFoundPage.Action
  | FlightSchedulingPageAction FlightSchedulingPage.Action
  | ReservationsPageAction ReservationsPage.Action
  | SignInCompleted Session.Session
  | SignOutCompleted 
  | NoOperation
  deriving (Eq)


foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = Wasm.run $ start


start :: JSM ()
start = do
  route <- Routes.currentRoute

  startApp $ App
    { initialAction = Initialise
    , model = initModel route
    , update = updateModel
    , view = viewModel
    , events = defaultEvents
    , subs = 
      [ Routes.subscribeToChanges RouteChanged
      , Session.subscribeToSignedIn SignInCompleted 
      , Session.subscribeToSignedOut SignOutCompleted
      ]
    , mountPoint = Nothing
    , logLevel = Off
    }


initModel :: Maybe Routes.Route -> Model
initModel route = 
  Initialising { route = route, header = Header.initModel route Nothing }


updateModel :: Action -> Model -> Effect Action Model
-- initialising
updateModel Initialise model@(Initialising {..}) =
  case route of
    Nothing -> 
      Effect.return 
        (model { route = Just Routes.defaultRoute, header = Header.initModel (Just Routes.defaultRoute) Nothing }) 
        (Routes.pushState Routes.defaultRoute Initialise)
    
    pageRoute ->
      Effect.return 
        (NotSignedIn { route = pageRoute, header = Header.initModel pageRoute Nothing })
        (const NoOperation <$> Session.restore)

updateModel (RouteChanged path) model@(Initialising {}) =
  return (model { route = newRoute, header = Header.initModel newRoute Nothing })
    where 
      newRoute = Routes.fromText path

updateModel (HeaderAction action) model@(Initialising {..}) =
  Header.updateModel action header & Effect.map (setHeader model) HeaderAction

updateModel _ model@(Initialising {}) =
  return model

-- not signed in
updateModel (SignInModalAction SignInModal.SignInDemo) model@(NotSignedIn {}) =
  Effect.return model (const NoOperation <$> Session.signInDemo)

updateModel (SignInCompleted session) (NotSignedIn {..}) =
  initPage route model'
    where 
      model' = SignedIn { route = route, page = NotFound pageModel, header = Header.initModel Nothing (Just session), session = session }
      Effect pageModel _ = NotFoundPage.initModel

updateModel _ model@(NotSignedIn {}) =
  return model

-- signed in
updateModel (RouteChanged path) model@(SignedIn {}) =
  initPage (Routes.fromText path) model

updateModel (HeaderAction action) model@(SignedIn {..}) =
  Header.updateModel action header & Effect.map (setHeader model) HeaderAction

updateModel (SignInCompleted session) model@(SignedIn {}) =
  return (model { session = session })

updateModel SignOutCompleted (SignedIn {..}) =
  return (NotSignedIn { route = route, header = Header.initModel route Nothing })

updateModel (NotFoundPageAction pageAction) model@(SignedIn { page }) =
  case page of
    NotFound pageModel ->
      NotFoundPage.updateModel pageAction pageModel & Effect.map (setPage model <$> NotFound) NotFoundPageAction
    _ ->
      return model

updateModel (FlightSchedulingPageAction pageAction) model@(SignedIn { page }) =
  case page of
    FlightScheduling pageModel ->
      FlightSchedulingPage.updateModel pageAction pageModel & Effect.map (setPage model <$> FlightScheduling) FlightSchedulingPageAction
    _ ->
      return model

updateModel (ReservationsPageAction pageAction) model@(SignedIn { page }) =
  case page of
    Reservations pageModel ->
      ReservationsPage.updateModel pageAction pageModel & Effect.map (setPage model <$> Reservations) ReservationsPageAction
    _ ->
      return model

updateModel _ model@(SignedIn {}) =
  return model


viewModel :: Model -> View Action
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

viewModel (NotSignedIn { header }) =
  div_ [ class_ "min-h-full animate-loaded" ] 
  [ div_ [ ] 
    [ HeaderAction <$> Header.viewModel header
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
    [ SignInModalAction <$> SignInModal.viewModel
    ]
  ]

viewModel (SignedIn { header, page }) =
  div_ [ class_ "min-h-full animate-loaded" ] 
  [ HeaderAction <$> Header.viewModel header
  , div_ [ class_ "p-8 -mt-64" ]
    [ h1_ [ class_ "text-3xl text-white"]
      [ text (pageTitle page)
      ]
    , div_ [ class_ "mx-auto pt-8 pb-12" ]
      [ div_ [ class_ "rounded-lg bg-white px-5 py-6 shadow min-h-[50vh]" ]
        [ viewPage page
        ]
      ]
    ]
  ]


viewPage :: Page -> View Action
viewPage (NotFound model) = 
  NotFoundPageAction <$> NotFoundPage.viewModel model

viewPage (FlightScheduling model) = 
  FlightSchedulingPageAction <$> FlightSchedulingPage.viewModel model

viewPage (Reservations model) = 
  ReservationsPageAction <$> ReservationsPage.viewModel model


initPage :: Maybe Routes.Route -> Model -> Effect Action Model
initPage pageRoute@(Nothing) model@(SignedIn {..}) = 
  Effect 
    (model { route = pageRoute, page = NotFound pageModel, header = Header.initModel pageRoute (Just session) }) 
    (mapSub NotFoundPageAction <$> pageAction)
      where 
        Effect pageModel pageAction = NotFoundPage.initModel

initPage pageRoute@(Just Routes.FlightScheduling) model@(SignedIn {..}) = 
  Effect 
    (model { route = pageRoute, page = FlightScheduling pageModel, header = Header.initModel pageRoute (Just session) }) 
    (mapSub FlightSchedulingPageAction <$> pageAction)
      where 
        Effect pageModel pageAction = FlightSchedulingPage.initModel session

initPage pageRoute@(Just Routes.Reservations) model@(SignedIn {..}) = 
  Effect 
    (model { route = pageRoute, page = Reservations pageModel, header = Header.initModel pageRoute (Just session) }) 
    (mapSub ReservationsPageAction <$> pageAction)
      where 
        Effect pageModel pageAction = ReservationsPage.initModel session

initPage _ model = return model


setHeader :: Model -> Header.Model -> Model 
setHeader model header = 
  model { header = header }


setPage :: Model -> Page -> Model 
setPage model@(SignedIn {}) page = model { page = page }
setPage model _ =  model


pageTitle :: Page -> Text
pageTitle (NotFound _) = "Page not found"
pageTitle (FlightScheduling _) = "Flight scheduling"
pageTitle (Reservations _) = "Reservations"
