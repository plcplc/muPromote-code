-- | This module contains auxiliary functions for using the http-client library.
module MuPromote.Common.HTTP.Client (
  socketManagerSettings
  ) where

import Network.Socket.ByteString ( recv, sendAll )
import Network.Socket ( sClose, Socket )
import Network.HTTP.Client ( ManagerSettings(..) )
import Network.HTTP.Client.Internal ( Connection, defaultManagerSettings, makeConnection )

-- | Construct a 'ManagerSettings' that just uses a given 'Socket'.
socketManagerSettings :: Socket -> ManagerSettings
socketManagerSettings sock = defaultManagerSettings {
  managerRawConnection = return $ \_ _ _ -> socketConnection sock
  }

-- | Stolen from http-client sources.
socketConnection :: Socket -> IO Connection
socketConnection socket = makeConnection
    (recv socket 4096)
    (sendAll socket)
    (sClose socket)
