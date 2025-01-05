module Extensions.Effect
  ( return
  , map
  ) where

import Prelude hiding (return, map)
import Miso


return :: model -> JSM action -> Effect action model
{-# INLINE return #-}
return model action =
  model <# do action


map :: (modelA -> modelB) -> (actionA -> actionB) -> Effect actionA modelA -> Effect actionB modelB
{-# INLINE map #-}
map mapModel mapAction (Effect ma as) = 
  Effect (mapModel ma) (mapSub mapAction <$> as) 