module Pages.NotFound
  ( notFound
  ) where

import Miso


data Model 
  = Ready {}
  deriving (Eq)


data Action 
  = NoOperation
  deriving (Eq)


notFound :: Component parent props Model Action
notFound = 
  component initModel updateModel viewModel


initModel :: Model
initModel = Ready


updateModel :: Action -> Effect parent props Model Action
updateModel =
  noop


viewModel :: props -> Model -> View Model Action
viewModel _ _ =
  text "Page cannot be found"