module Components.Popover
  ( PopoverOrientation(..)
  , popover
  ) where

import Miso
import Miso.Html.Element (nodeHtml)


data PopoverOrientation = OrientateLeft | OrientateRight


popover :: MisoString -> PopoverOrientation -> [View model action] -> View model action 
popover for_ orientation =
  nodeHtml "x-popover"
  [ textProp "for" for_
  , textProp "orientation" (formatOrientation orientation)
  ]
  where 
    formatOrientation OrientateLeft = "left"
    formatOrientation OrientateRight = "right"