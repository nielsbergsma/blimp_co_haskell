module Routes
  ( Route(..)
  , defaultRoute
  , fromText
  , toText
  , currentRoute
  , pushState
  , subscribeToChanges
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, unpack)
import Data.Aeson ((.:), withObject)
import GHC.Wasm.Prim
import Miso (Decoder(..), DecodeTarget(..), Sub, windowSub)


data Route
  = FlightScheduling
  | Reservations
  deriving (Eq)


fromText :: Text -> Maybe Route
fromText "/flight-scheduling" = Just FlightScheduling
fromText "/reservations" = Just Reservations
fromText _ = Nothing


toText :: Route -> Text
toText FlightScheduling = "/flight-scheduling"
toText Reservations = "/reservations"


defaultRoute :: Route
defaultRoute = FlightScheduling


-- helpers
currentRoute :: MonadIO m => m (Maybe Route)
currentRoute = do
  pathname <- liftIO $ js_location_pathname
  return . fromText . pack . fromJSString $ pathname


pushState :: MonadIO m => Route -> action -> m action
pushState route action = do 
  _ <- liftIO . js_history_pushstate . toJSString . unpack . toText $ route
  return action


subscribeToChanges :: (Text -> action) -> Sub action
subscribeToChanges action = windowSub "routechanged" decoder action
  where
    decoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = withObject "Event" $ \event -> event .: "detail"
      }
      

-- foreign imports / exports
foreign import javascript safe "history.pushState({}, '', $1);"
  js_history_pushstate :: JSString -> IO ()


foreign import javascript safe "return window.location.pathname;"
  js_location_pathname :: IO JSString