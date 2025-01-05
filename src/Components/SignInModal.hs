module Components.SignInModal
  ( Action(..)
  , viewModel
  ) where

import Miso
import Components.Icon as Icon

data Action 
  = SignInDemo
  deriving (Eq)


viewModel :: View Action
viewModel = 
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