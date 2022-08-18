module Curly.CurlhsExtras where

import Coalmine.Prelude
import qualified Data.ByteString as ByteString
import Network.CURL730

-- | Recursively updates the read function until the whole bytestring is consumed.
setByteStringReadFunction :: CURL -> ByteString -> IO ()
setByteStringReadFunction handle input =
  if ByteString.null input
    then
      curl_easy_setopt handle $
        [ CURLOPT_READFUNCTION Nothing
        ]
    else
      curl_easy_setopt handle $
        [ CURLOPT_READFUNCTION . Just $ \requestedAmount ->
            case ByteString.splitAt requestedAmount input of
              (toSend, remainder) -> do
                setByteStringReadFunction handle remainder
                return $ CURL_READFUNC_OK toSend
        ]
