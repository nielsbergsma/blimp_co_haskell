module Pages.NotFound
  ( Model (..) 
  , Action (..)
  , initModel
  , updateModel
  , viewModel
  ) where

import Miso


data Model 
  = Ready {}
  deriving (Eq)


data Action 
  = NoOperation
  deriving (Eq)


initModel :: Effect Action Model
initModel = return Ready


updateModel :: Action -> Model -> Effect Action Model
updateModel NoOperation model = 
  return model


viewModel :: Model -> View Action
viewModel _ = text "Page cannot be found"
