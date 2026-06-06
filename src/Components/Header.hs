module Components.Header
  ( header
  , Props
  ) where

import Miso
import Miso.Html.Element (ul_, li_, button_, div_, span_,  nav_, img_)
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_, src_)

import Session
import Routes
import Components.Icon qualified as Icon


-- | Parent-supplied state: the current route and (optional) signed-in session.
--   Passed down as props, so changes re-render the header in place without remounting.
type Props = (Maybe Routes.Route, Maybe Session)


newtype Model = Model
  { profileMenuOpen :: Bool
  }
  deriving (Eq)


data Action
  = ToggleProfileMenu
  | RedirectToPage Routes.Route
  | SignOut
  deriving (Eq)


header :: Component parent Props Model Action
header =
  component initModel updateModel viewModel


initModel :: Model
initModel =
  Model { profileMenuOpen = False }


updateModel :: Action -> Effect parent Props Model Action
updateModel = \case
  ToggleProfileMenu ->
    modify $ \model -> model { profileMenuOpen = not model.profileMenuOpen }

  RedirectToPage route ->
    redirect route

  SignOut -> do
    signOut
    modify $ \model -> model { profileMenuOpen = False }


viewModel :: Props -> Model -> View Model Action
viewModel (route, session) model =
  div_ [ class_ "bg-gray-800 pb-64" ]
  [ nav_ [ class_ "bg-gray-800" ] 
    [ div_ [ class_ "mx-auto lg:px-8" ]
      [ div_ [ class_ "border-b border-gray-700"]
        [ div_ [ class_ "flex h-16 items-center justify-between px-4 sm:px-0" ]
          [ div_ [ class_ "flex items-center" ]
            [ div_ [ class_ "flex-shrink-0" ]
              [ Icon.logo [ class_ "h-12 w-12 align-[-2.5rem] text-white" ]
              ]
            ]

          , viewMenu route

          , div_ [ class_ "relative" ]
            [ button_ [ class_ "flex flex-row items-center text-white rounded-full hover:bg-gray-700 hover:text-white", onClick ToggleProfileMenu ]
              [ div_ [ class_ "ml-4 flex items-center text-sm mx-4 relative" ]
                [ case session of
                    Just currentSession -> text currentSession.name
                    Nothing -> text "(not signed in)"
                ]
              , div_ [ class_ "w-8 h-8 bg-white text-gray-800 rounded-full flex items-center justify-center" ]
                [ case session >>= photoUrl of
                    Just url -> img_ [ class_ "mx-4 h-7 w-7 rounded-full", src_ url ]
                    Nothing -> Icon.user [ class_ "h-7 w-7" ]
                ]
              ]
            , if model.profileMenuOpen
                then viewProfileMenu
                else noHtml
            ]
          ]
        ]
      ]
    ]
  ]

viewMenu :: Maybe Routes.Route -> View Model Action
viewMenu route =
  div_ [ class_ "ml-5 flex-grow flex flex-row" ]
  [ viewMenuItem route Routes.FlightScheduling "Flight scheduling" Icon.compass
  , viewMenuItem route Routes.Reservations "Reservations" Icon.receipt
  ]


viewMenuItem :: Maybe Routes.Route -> Routes.Route -> MisoString -> Icon.Icon Model Action -> View Model Action
viewMenuItem route destination name icon =
  if route == (Just destination)
  then
    div_ [ class_ "mx-2 flex items-baseline space-x-4" ]
    [ span_ [ class_ "bg-gray-900 text-white rounded-md px-3 py-2 text-sm font-medium" ]
      [ icon [ class_ "w-4 h-4 mr-2" ]
      , text name
      ]
    ]
  else
    div_ [ class_ "mx-2 flex items-baseline space-x-4" ]
    [ button_ [ onClick (RedirectToPage destination), class_ "text-gray-300 rounded-md px-3 py-2 text-sm font-medium hover:bg-gray-700 hover:text-white" ]
      [ icon [ class_ "w-4 h-4 mr-2" ]
      , text name
      ]
    ]


viewProfileMenu :: View Model Action
viewProfileMenu = 
  ul_ [ class_ "absolute bg-white top-9 right-1 w-48 p-2 text-sm rounded-md shadow-xl" ]
  [ li_ [ ] 
    [ button_ [ class_ "text-gray-800 py-1 px-2 rounded w-full hover:bg-gray-300 text-left", onClick SignOut ] 
      [ Icon.key [ class_ "h-4 w-4 mr-2" ]
      , text "Sign out" 
      ]
    ]
  ]


noHtml :: View Model Action
noHtml = text ""