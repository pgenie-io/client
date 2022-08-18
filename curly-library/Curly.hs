module Curly
  ( -- * Handle
    Handle,
    acquireHandle,

    -- * Op
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude hiding (Handle, Op, Version)
import qualified Network.CURL730 as Curl

-- * Handle

newtype Handle
  = Handle Curl.CURL

-- | Acquire reusable handle,
-- which can be used to execute sessions.
acquireHandle :: IO Handle
acquireHandle =
  error "TODO"

-- * Op

newtype Op a
  = Op (Curl.CURL -> IO (Either Curl.CURLE a))
