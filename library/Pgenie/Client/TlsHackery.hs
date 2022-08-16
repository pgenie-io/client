module Pgenie.Client.TlsHackery where

import Coalmine.Prelude
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client.TLS
import qualified Network.TLS as Tls
import qualified Network.TLS.Extra.Cipher as TlsCipher
import qualified System.X509 as X509

-- |
-- Acquire a more resilient TLS manager.
acquireManager host port =
  Network.HTTP.Client.TLS.newTlsManagerWith
    =<< acquireManagerSettings host port

acquireManagerSettings host port =
  Network.HTTP.Client.TLS.mkManagerSettings
    <$> acquireTlsSettings host port
    <*> pure Nothing

acquireTlsSettings host port =
  Connection.TLSSettings <$> acquireClientParams host port

acquireClientParams host port = do
  certificateStore <- X509.getSystemCertificateStore
  return $
    Tls.ClientParams
      { clientUseMaxFragmentLength = Just Tls.MaxFragment4096,
        clientServerIdentification = (to host, encodeUtf8 . showAs $ port),
        clientUseServerNameIndication = True,
        clientWantSessionResume = Nothing,
        clientShared =
          def
            { Tls.sharedCAStore = certificateStore
            },
        clientHooks =
          def
            { Tls.onSuggestALPN = 
                return . Just . pure $ "h2",
              Tls.onCertificateRequest =
                \args -> do
                  traceShowM ("onCertificateRequest", args)
                  Tls.onCertificateRequest def args,
              Tls.onCustomFFDHEGroup =
                \params public -> do
                  traceShowM ("onCustomFFDHEGroup", params, public)
                  Tls.onCustomFFDHEGroup def params public
            },
        clientSupported =
          def
            { Tls.supportedVersions = [Tls.TLS13, Tls.TLS12, Tls.TLS11, Tls.TLS10],
              Tls.supportedCiphers = TlsCipher.ciphersuite_default
            },
        clientDebug = def,
        clientEarlyData = Nothing
      }
