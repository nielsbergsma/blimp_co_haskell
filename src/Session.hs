module Session
  ( Session(..)
  , signInDemo
  , signOut
  , subscribeToSignedIn
  , subscribeToSignedOut
  , restore
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson 
import Data.Aeson.Types
import Data.Text (Text)
import Miso (Decoder(..), DecodeTarget(..), Sub, windowSub)


data Session 
  = Session { name :: Text, photoUrl :: Maybe Text, token :: Text }
  deriving (Eq)


signInDemo :: MonadIO m => m ()
signInDemo = liftIO jsSignInDemo


subscribeToSignedIn :: (Session -> action) -> Sub action
subscribeToSignedIn action = windowSub "signedin" decoder action
  where
    decoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = eventDetailDecoder
      }


signOut :: MonadIO m => m ()
signOut = liftIO jsSignOut


subscribeToSignedOut :: action -> Sub action
subscribeToSignedOut action = windowSub "signedout" decoder (const action)
  where
    decoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = eventDetailDecoder :: Value -> Parser ()
      }


restore :: MonadIO m => m ()
restore = liftIO jsRestoreSession
  

-- decoders
eventDetailDecoder :: FromJSON a => Value -> Parser a
eventDetailDecoder = 
  withObject "Event" $ \event -> do
    detail <- event .: "detail"

    case eitherDecodeStrictText detail of 
      Left error -> fail error 
      Right value -> return value


instance FromJSON Session where
  parseJSON (Object value) = 
    Session
      <$> value .: "name"
      <*> value .: "photoUrl"
      <*> value .: "token"
  
  parseJSON value = 
    unexpected value


-- foreign imports / exports
foreign import javascript safe "signInDemo();"
  jsSignInDemo :: IO ()

foreign import javascript safe "signOut();"
  jsSignOut :: IO ()

foreign import javascript safe "restoreSession();"
  jsRestoreSession :: IO ()
