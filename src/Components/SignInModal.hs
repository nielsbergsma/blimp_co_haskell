module Components.SignInModal
  ( signInModal
  ) where

import Miso
import Miso.Html.Element (div_, button_, h1_, h2_)
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Components.Icon as Icon
import Session (signInDemo)

signInModal :: Component parent Model Action
signInModal = 
  component initModel updateModel viewModel

data Model = Model
  deriving (Eq)

initModel :: Model
initModel = 
  Model

data Action 
  = SignInDemo
  deriving (Eq)

updateModel :: Action -> Effect parent Model Action
updateModel SignInDemo = 
  signInDemo

viewModel :: Model -> View Model Action
viewModel _ = 
  div_ [ class_ "bg-gray-800 w-[48rem] shadow-2xl rounded-b-lg text-white p-8" ]
  [ h1_ [ class_ "text-2xl mb-4" ] 
    [ text "Welcome to Blimp & Co"
    ]
  , h2_ [ class_ "text-xl mb-4"]
    [ text "Please sign in"
    ]
  , div_ [ class_ "flex justify-center m-8" ]
    [ div_ []
      [ button_ [ class_ "rounded-full hover:text-gray-800 hover:bg-white h-32 w-32 p-4", onClick SignInDemo ]
        [ div_ [] 
          [ Icon.key [ class_ "w-10 h-10" ]
          ]
        , div_ [] 
          [ text "Sign in as administrator" 
          ]
        ]
      ]
    ]
  ]