-- |
-- A thin wrapper over lean http client.
module Pgenie.Client
  ( -- * Execution
    Lhc.Err (..),
    run,

    -- * Operations
    Op,
    process,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude hiding (Op, Version)
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Data.Text.IO as TextIO
import qualified LeanHttpClient as Lhc
import qualified Pgenie.Client.TlsHackery as TlsHackery
import qualified Pgenie.Protocol as Protocol
import qualified System.Directory as Directory

-- * Operations

-- | Execute operation.
run :: Op a -> Bool -> Text -> Maybe Int -> IO (Either Lhc.Err a)
run (Op op) secure host port = do
  manager <-
    TlsHackery.acquireManager host (bool 80 443 secure)
  runReaderT op (secure, Lhc.textHost host, port)
    & flip Lhc.runSession manager

newtype Op a
  = Op (ReaderT (Bool, Lhc.Host, Maybe Int) Lhc.Session a)
  deriving (Functor, Applicative, Monad)

executeRequest :: Protocol.Request -> Op Protocol.Response
executeRequest req =
  Op . ReaderT $ \(https, host, port) -> do
    traceM $ "Sending " <> show (ByteString.length requestBody) <> " bytes"
    Lhc.overrideTimeout 15 $
      Lhc.post (url https host port) headers requestBody parser
  where
    url https host port =
      Lhc.url https host port path query
      where
        path = "/v1"
        query = []
    headers =
      mconcat
        [ Lhc.requestHeader "content-type" "application/octet-stream"
        ]
    requestBody =
      Cereal.encode req
    parser = do
      Lhc.expectOkStatus
      Lhc.deserializeBodyWithCereal Cereal.get

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
