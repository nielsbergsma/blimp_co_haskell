module Components.Popover
  ( PopoverOrientation(..)
  , popover
  ) where

import Data.Text (Text)
import Miso


data PopoverOrientation = OrientateLeft | OrientateRight


popover :: Text -> PopoverOrientation -> [View action] -> View action 
popover for orientation =
  nodeHtml "x-popover"
  [ textProp "for" for
  , textProp "orientation" (formatOrientation orientation)
  ]
  where 
    formatOrientation OrientateLeft = "left"
    formatOrientation OrientateRight = "right"