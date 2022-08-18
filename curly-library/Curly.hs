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
  = Handle (TQueue Curl.CURL)

-- | Acquire handle pool,
-- which can be used to execute sessions.
acquireHandle :: Int -> IO Handle
acquireHandle =
  error "TODO"

-- * Op

newtype Op a
  = Op (Curl.CURL -> IO (Either Curl.CURLE a))

post ::
  Url ->
  [(Text, Text)] ->
  ByteString ->
  HeadersParser (BodyParser a) ->
  Op a
post =
  error "TODO"

-- * Url

data Url
  = Url
      !Bool
      !(BVec Text)
      !Path.Path

-- * HeadersParser

data HeadersParser a

-- * BodyParser

data BodyParser a
