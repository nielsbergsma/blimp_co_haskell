module Runtime.Fetch 
  ( HttpRequest(..)
  , HttpResponse(..)
  , fetch
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson hiding (Success, Error)
import Data.Aeson.Types (unexpected)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Text (Text, pack)
import GHC.Wasm.Prim (JSString(..), toJSString, fromJSString)


data HttpRequest 
  = HttpRequest { method :: Text, url :: Text {-, body :: ... -} }

data HttpResponse a
  = Success { status :: Int, body :: a {-, headers :: ... -} }
  | Error { error :: Text }


fetch :: (FromJSON a, MonadIO m) => HttpRequest -> m (HttpResponse a)
fetch request = do
  response <- liftIO $ pack . fromJSString <$> js_fetch (toJSString . UTF8.toString . encode $ request)
  case eitherDecodeStrictText response of
    Left error -> 
      return (Error (pack error))

    Right value -> 
      return value


-- decoders
instance ToJSON HttpRequest where
  toJSON (HttpRequest method url) = object
    [ "method" .= method
    , "url" .= url
    ]

instance (FromJSON a) => FromJSON (HttpResponse a) where
  parseJSON (Object value) = 
    (Error <$> value .: "error") <|> (Success <$> value .: "status" <*> value .: "body")
  
  parseJSON value = 
    unexpected value


-- foreign imports / exports
foreign import javascript safe "try { const o = JSON.parse($1); const r = await fetch(o.url.replace('@backend', backendUrl), o); return JSON.stringify({ status: r.status, headers: r.headers, body: await r.json() }); } catch(e) { return { error: e.toString()}; };"
  js_fetch :: JSString -> IO JSString
