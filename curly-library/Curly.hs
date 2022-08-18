module Curly
  ( runOpHappily,

    -- * Op
    Op,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude hiding (Handle, Op, Version)
import qualified Curly.CurlhsExtras as CurlhsExtras
import qualified Data.Serialize as Cereal
import qualified Network.CURL730 as Curl

runOp :: Op a -> IO (Either OpErr a)
runOp (Op runOp) = do
  curl <- Curl.curl_easy_init
  Curl.curl_easy_setopt curl [Curl.CURLOPT_FOLLOWLOCATION True]
  res <- runOp curl
  Curl.curl_easy_cleanup curl
  return res

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
  -- | URL.
  String ->
  -- | Request body.
  ByteString ->
  -- | Response body parser.
  BodyParser body ->
  Op body
post url body (BodyParser setBodyParserUp) =
  Op $ \curl -> runExceptT $ do
    lift $ Curl.curl_easy_setopt curl [Curl.CURLOPT_URL url, Curl.CURLOPT_POST True]
    lift $ CurlhsExtras.setByteStringReadFunction curl body
    readBodyResult <- lift $ setBodyParserUp curl
    ExceptT $ catch (Right <$> Curl.curl_easy_perform curl) (return . Left . CurlOpErr)
    ExceptT $ first (BodyParserOpErr . to) <$> readBodyResult

-- * BodyParser

newtype BodyParser a
  = BodyParser (Curl.CURL -> IO (IO (Either String a)))

explicitCerealBodyParser :: Cereal.Get a -> BodyParser a
explicitCerealBodyParser get =
  BodyParser $ \curl -> do
    CurlhsExtras.setAwatingCerealWriteFunction curl get

implicitCerealBodyParser :: Cereal.Serialize a => BodyParser a
implicitCerealBodyParser = explicitCerealBodyParser Cereal.get
