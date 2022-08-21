-- |
-- A thin wrapper over lean http client.
module Pgenie.Client
  ( -- * Execution
    runOpHappily,

    -- * Operations
    Op,
    process,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude hiding (Op, Version)
import qualified Curly
import qualified Data.Serialize as Cereal
import qualified Data.Text.IO as TextIO
import qualified Pgenie.Protocol as Protocol
import qualified System.Directory as Directory

-- * Operations

-- | Execute operation.
runOpHappily :: Op a -> Bool -> Text -> Maybe Int -> IO a
runOpHappily (Op op) secure host port = do
  runReaderT op url
    & Curly.runOpHappily 15
  where
    url =
      bool "http://" "https://" secure
        <> to host
        <> foldMap (mappend ":" . show) port
        <> "/v1"

newtype Op a
  = Op (ReaderT String Curly.Op a)
  deriving (Functor, Applicative, Monad)

executeRequest :: Protocol.Request -> Op Protocol.Response
executeRequest req =
  Op . ReaderT $ \url ->
    Curly.post url headers requestBody Curly.implicitCerealBodyParser
  where
    headers =
      [ ("content-type", "application/octet-stream")
      ]
    requestBody =
      Cereal.encode req
    parser = Curly.implicitCerealBodyParser

process :: Protocol.Version -> Word -> Text -> BVec (Path, Text) -> BVec (Path, Text) -> Op (Either Text (BVec (Path, Text)))
process clientVersion configVersion configContents migrations queries = do
  fmap mapOut $ executeRequest request
  where
    request =
      Protocol.ProcessRequest $
        Protocol.RequestProcess clientVersion configVersion configContents migrations queries
    mapOut = \case
      Protocol.FailedResponse err -> Left err
      Protocol.GeneratedResponse res -> Right res
