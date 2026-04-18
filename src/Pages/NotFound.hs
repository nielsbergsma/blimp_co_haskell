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


notFound :: Component parent Model Action
notFound = 
  component initModel updateModel viewModel


initModel :: Model
initModel = Ready


updateModel :: Action -> Effect parent Model Action
updateModel = 
  noop


viewModel :: Model -> View Model Action
viewModel _ = 
  text "Page cannot be found"