module Components.Header 
  ( Model
  , Action(..)
  , initModel
  , updateModel
  , viewModel
  ) where

import Data.Text (Text)
import Miso

import Session
import qualified Extensions.Effect as Effect
import qualified Routes
import qualified Components.Icon as Icon


data Model = Model
  { route :: Maybe Routes.Route
  , session :: Maybe Session
  , profileMenuOpen :: Bool
  }
  deriving (Eq)


data Action
  = ToggleProfileMenu
  | SignOut
  | SignedOut
  deriving (Show, Eq)


initModel :: Maybe Routes.Route -> Maybe Session -> Model
initModel route session = Model { route = route, session = session, profileMenuOpen = False }


updateModel :: Action -> Model -> Effect Action Model
updateModel action model@(Model {..}) =
  case action of
    ToggleProfileMenu -> 
      return (model { profileMenuOpen = not profileMenuOpen })

    SignOut -> 
      Effect.return model (const SignedOut <$> signOut)

    SignedOut ->
      return (model { profileMenuOpen = False })


viewModel :: Model -> View Action
viewModel model =
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

          , viewMenu model
  
          , div_ [ class_ "relative" ] 
            [ button_ [ class_ "flex flex-row items-center text-white rounded-full hover:bg-gray-700 hover:text-white", onClick ToggleProfileMenu ]
              [ div_ [ class_ "ml-4 flex items-center text-sm mx-4 relative" ]
                [ case model.session of
                    Just session -> text session.name
                    Nothing -> text "(not signed in)" 
                ]
              , div_ [ class_ "w-8 h-8 bg-white text-gray-800 rounded-full flex items-center justify-center" ]
                [ case model.session >>= photoUrl of
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

viewMenu :: Model -> View Action
viewMenu model = 
  div_ [ class_ "ml-5 flex-grow flex flex-row" ]
  [ viewMenuItem model Routes.FlightScheduling "Flight scheduling" Icon.compass
  , viewMenuItem model Routes.Reservations "Reservations" Icon.receipt
  ]


viewMenuItem :: Model -> Routes.Route -> Text -> Icon.Icon Action -> View Action
viewMenuItem model destination name icon = 
  if model.route == (Just destination) 
  then
    div_ [ class_ "mx-2 flex items-baseline space-x-4" ]
    [ span_ [ class_ "bg-gray-900 text-white rounded-md px-3 py-2 text-sm font-medium" ]
      [ icon [ class_ "w-4 h-4 mr-2" ]
      , text name
      ]
    ]
  else
    div_ [ class_ "mx-2 flex items-baseline space-x-4" ]
    [ a_ [ href_ (Routes.toText destination), class_ "text-gray-300 rounded-md px-3 py-2 text-sm font-medium hover:bg-gray-700 hover:text-white" ]
      [ icon [ class_ "w-4 h-4 mr-2" ]
      , text name
      ]
    ]


viewProfileMenu :: View Action
viewProfileMenu = 
  ul_ [ class_ "absolute bg-white top-9 right-1 w-48 p-2 text-sm rounded-md shadow-xl" ]
  [ li_ [ ] 
    [ button_ [ class_ "text-gray-800 py-1 px-2 rounded w-full hover:bg-gray-300 text-left", onClick SignOut ] 
      [ Icon.key [ class_ "h-4 w-4 mr-2" ]
      , text "Sign out" 
      ]
    ]
  ]


noHtml :: View Action
noHtml = text ""