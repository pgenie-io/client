module Curly
  ( runOpHappily,

    -- * Op
    Op,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude hiding (Handle, Op, Version)
import qualified Network.CURL730 as Curl

runOp :: Op a -> IO (Either OpErr a)
runOp (Op runOp) =
  error "TODO"

runOpHappily :: Op a -> IO a
runOpHappily =
  error "TODO"

-- * Op

newtype Op a
  = Op (Curl.CURL -> IO (Either OpErr a))

data OpErr
  = CurlOpErr Curl.CURLE
  | BodyParserOpErr Text

post ::
  Url ->
  [(Text, Text)] ->
  ByteString ->
  HeadersParser headers ->
  BodyParser body ->
  Op (headers, body)
post Url {..} headers body _ (BodyParser initWriteFunction) =
  Op $ \curl -> runExceptT $ do
    (writeFunction, readBodyResult) <- lift initWriteFunction
    lift $
      Curl.curl_easy_setopt
        curl
        [ Curl.CURLOPT_WRITEFUNCTION . Just $ writeFunction
        ]
    headers <- error "TODO"
    ExceptT $ catch (Right <$> Curl.curl_easy_perform curl) (return . Left . CurlOpErr)
    body <- ExceptT $ first BodyParserOpErr <$> readBodyResult
    return (headers, body)

-- * Url

data Url = Url
  { urlSecure :: !Bool,
    urlDomain :: !(BVec Text),
    urlPath :: !Path.Path
  }

-- * HeadersParser

data HeadersParser a

-- * BodyParser

newtype BodyParser a
  = BodyParser (IO (ByteString -> IO Curl.CURL_write_response, IO (Either Text a)))
