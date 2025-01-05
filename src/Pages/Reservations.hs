module Pages.Reservations
  ( Model (..)
  , Action (..)
  , initModel
  , updateModel
  , viewModel
  ) where

import Miso
import Session
import qualified Components.Icon as Icon


data Model
  = Initialising
  deriving (Eq)


data Action 
  = NoOperation
  deriving (Eq)


initModel :: Session -> Effect Action Model
initModel _ = return Initialising


updateModel :: Action -> Model -> Effect Action Model
updateModel NoOperation model = 
  return model


viewModel :: Model -> View Action
viewModel _ = 
  div_ [ class_ "flex justify-center items-center" ] 
  [ div_ [ class_ "bg-gray-800 text-white w-96 p-4 -mt-6 rounded-b-md text-center" ] 
    [ Icon.spinner [ class_ "w-4 h-4 mr-2" ]
    , text "Fetching reservations"
    ]
  ]
