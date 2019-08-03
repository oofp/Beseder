module Beseder.Resources.Comm 
  ( module Beseder.Resources.Comm.CommProv
  , module Beseder.Resources.Comm.Impl.Console
  , module Beseder.Resources.Comm.Impl.WSClient
  ) where 

import Beseder.Resources.Comm.CommProv
  ( CommRes (..)
  , SendMsg (..)
  , GetNextMsg (..)
  , CloseComm (..)
  , CommProv 
  , CommInitiated 
  , CommWaitForMsg 
  , CommMsgRcvd 
  , CommClosed 
  , CommFailed 
  , IsMessageReceived
  , IsCommAlive 
  , getCommFailure
  , getIncomingMsg
  ) 
import Beseder.Resources.Comm.Impl.Console
import Beseder.Resources.Comm.Impl.WSClient

