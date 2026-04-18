module Pages.Reservations
  ( reservations
  ) where

import Miso
import Miso.Html.Element (div_)
import Miso.Html.Property (class_)

import Session
import Components.Icon qualified as Icon


data Model
  = Initialising
  deriving (Eq)


data Action 
  = NoOperation
  deriving (Eq)


reservations :: Session -> Component parent Model Action
reservations session = 
  component (initModel session) updateModel viewModel


initModel :: Session -> Model
initModel _ = 
  Initialising


updateModel :: Action -> Effect parent Model Action 
updateModel = 
  noop


viewModel :: Model -> View Model Action
viewModel _ = 
  div_ [ class_ "flex justify-center items-center" ] 
  [ div_ [ class_ "bg-gray-800 text-white w-96 p-4 -mt-6 rounded-b-md text-center" ] 
    [ Icon.spinner [ class_ "w-4 h-4 mr-2" ]
    , text "Fetching reservations"
    ]
  ]