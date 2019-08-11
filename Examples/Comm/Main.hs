{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  Main where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import qualified Data.ByteString.Lazy as LBStr                                               
import qualified Network.WebSockets  as WS
import           Data.Function (id)
import           Control.Monad.Cont (ContT)
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Comm 

import           CommApps

echoWS :: WSClient 
echoWS = WSClient
  { host = "echo.websocket.org"
  , port = 80
  , path = "/"
  , options = WS.defaultConnectionOptions
  , headers = [] 
  , flEnablePing = False
  }     

commRes1 :: CommRes WSClient LBStr.ByteString Text ()
commRes1 = CommRes echoWS

commRes2 :: ConsoleRes 
commRes2 = consoleRes

proxyWsConsoleApp :: STransApp (ContT Bool) TaskQ NoSplitter '[()] '(('[()]),'[]) ()
proxyWsConsoleApp = 
  proxyApp commRes1 commRes2 show id ((==)"q")

main :: IO ()
main = runAsyncApp proxyWsConsoleApp

