module Main where

import Coalmine.Prelude
import qualified Data.ByteString.Char8 as ByteString
import Network.Connection

main = do
  ctx <- initConnectionContext
  con <-
    connectTo ctx $
      ConnectionParams
        { connectionHostname = "api.pgenie.io",
          connectionPort = 443,
          connectionUseSecure = Just $ def,
          connectionUseSocks = Nothing
        }
  traceM "+ send"
  connectionPut con $ ByteString.replicate 10000 'z'
  traceM "- send"
  traceM "+ recv"
  r <- connectionGet con 200
  traceM "- recv"
  ByteString.putStrLn r
  connectionClose con
