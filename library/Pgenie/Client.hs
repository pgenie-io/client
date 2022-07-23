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
import Coalmine.Prelude hiding (Op)
import qualified Data.Serialize as Cereal
import qualified Data.Text.IO as TextIO
import qualified LeanHttpClient as Lhc
import qualified Pgenie.Protocol.V1 as Protocol
import qualified System.Directory as Directory

-- | Execute operation.
run :: Op a -> Bool -> Text -> Maybe Int -> IO (Either Lhc.Err a)
run (Op op) secure host port = do
  runReaderT op (secure, Lhc.textHost host, port)
    & Lhc.runSessionOnGlobalManager

newtype Op a
  = Op (ReaderT (Bool, Lhc.Host, Maybe Int) Lhc.Session a)
  deriving (Functor, Applicative, Monad)

-- * Operations

executeRequest :: Protocol.Request -> Op Protocol.Response
executeRequest req =
  Op . ReaderT $ \(https, host, port) ->
    Lhc.post (url https host port) headers requestBody parser
  where
    url https host port =
      Lhc.url https host port path query
      where
        path = "/api/v1"
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

process :: Name -> Name -> BVec (Path, Text) -> BVec (Path, Text) -> BVec Name -> Op (Either Text (BVec (Path, Text)))
process space name migrations queries artifacts = do
  fmap mapOut $ executeRequest request
  where
    request =
      Protocol.ProcessRequest $
        Protocol.RequestProcess space name migrations queries artifacts
    mapOut = \case
      Protocol.FailedResponse err -> Left err
      Protocol.GeneratedResponse res -> Right res
