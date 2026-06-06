{-# LANGUAGE QuasiQuotes #-}

module Session
  ( Session(..)
  , signInDemo
  , signOut
  , subscribeToSignedIn
  , subscribeToSignedOut
  , restore
  ) where

import Miso (Decoder(..), DecodeTarget(..), Effect, Sub, windowSub, MisoString, fromMisoString, io_)
import Miso.FFI.QQ (js)
import Miso.JSON

data Session = Session
  { name :: MisoString
  , photoUrl :: Maybe MisoString
  , token :: MisoString
  } deriving (Eq)


signInDemo :: Effect parent props model action
signInDemo = io_ [js| signInDemo() |]

subscribeToSignedIn :: (Session -> action) -> Sub action
subscribeToSignedIn action = windowSub "signedin" decoder action
  where
    decoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = eventDetailDecoder
      }


signOut :: Effect parent props model action
signOut = io_ [js| signOut() |]

subscribeToSignedOut :: action -> Sub action
subscribeToSignedOut action = windowSub "signedout" decoder (const action)
  where
    decoder = Decoder 
      { decodeAt = DecodeTarget mempty
      , decoder = eventDetailDecoder :: Value -> Parser ()
      }


restore :: Effect parent props model action
restore = io_ [js| restoreSession() |]

-- decoders
eventDetailDecoder :: FromJSON a => Value -> Parser a
eventDetailDecoder = 
  withObject "Event" $ \event -> do
    detail <- event .: "detail"

    case eitherDecode detail of 
      Left problem -> fail (fromMisoString problem)
      Right value -> return value


instance FromJSON Session where
  parseJSON (Object value) = 
    Session
      <$> value .: "name"
      <*> value .: "photoUrl"
      <*> value .: "token"
  
  parseJSON value = 
    typeMismatch "expected object" value
