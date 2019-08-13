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
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  CommFuncApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Control.Monad.Cont (ContT)                                               
import           Beseder.Base.Control                                               
import           Beseder.Misc.Misc                                               
import           Beseder.Base.Common                                               
import           Beseder.Resources.Comm 
import           Beseder.Resources.State.DataRes (InitAsTrue, SetFalse, IsTrue)
import           Data.String 
import qualified Data.ByteString.Lazy as LBStr                                               
import qualified Network.WebSockets  as WS

type CompletedRes = '(('[()]),'[])

-- :kind! EvalTransFunc IO (ProxyApp Console WSClient Text Text Text LBStr.ByteString () ())
-- :kind! EvalTransFuncWithTrace IO (ProxyApp Console WSClient Text Text Text LBStr.ByteString () ())

type ProxyApp commPars1 commPars2 i1 i2 o1 o2 e1 e2 m =
  "comRes1" >| NewRes "com1" (CommRes commPars1 i1 o1 e1) m 
  :>> "comRes2" >| NewRes "com2" (CommRes commPars2 i2 o2 e2) m
  :>> NewRes "flCont" InitAsTrue m
  :>> Trace "comm created"
  :>> Try(("com1" :? IsCommAlive) :&& ("com2" :? IsCommAlive) :&& ("flCont" :? IsTrue)) 
    ( HandleEvents
      ( On ("com1" :? IsMessageReceived)
            ( "shouldQuit" >| IffFunc (Invoke "flCont" SetFalse)
              :>> On ("com2" :? IsCommConnected) 
                ( Trace "com2Connected"
                :>> "rcvd1ToSend2" >| Invoke "com2" (SendMsg o2) 
                )
              :>> Invoke "com1" GetNextMsg
            )  
      :>> On ("com2" :? IsMessageReceived)
          ( On ("com1" :? IsCommConnected) 
              ( Trace "com1Connected"
              :>> "rcvd2ToSend1" >| Invoke "com1" (SendMsg o1) 
              )
            :>> Invoke "com2" GetNextMsg
          )
      )
    )
    :>> On ("com1" :? IsCommAlive) (Invoke "com1" CloseComm)
    :>> On ("com2" :? IsCommAlive) (Invoke "com2" CloseComm)
    :>> ClearAllResourcesButTrace
    

echoWS :: WSClient 
echoWS = WSClient
  { host = "echo.websocket.org"
  , port = 80
  , path = "/"
  , options = WS.defaultConnectionOptions
  , headers = [] 
  , flEnablePing = False
  }     

commRes2 :: CommRes WSClient LBStr.ByteString Text ()
commRes2 = CommRes echoWS

commRes1 :: ConsoleRes 
commRes1 = consoleRes

data ProxyAppDict = ProxyAppDict

instance TransDict q m ProxyAppDict "comRes2" xs (CommRes WSClient LBStr.ByteString Text ()) where
  getTransFromDict dict _  =  MkApp $ return commRes2
instance TransDict q m ProxyAppDict "comRes1" xs ConsoleRes where
  getTransFromDict dict _ =  MkApp $ return commRes1

getConsoleMsg :: 
  (Has (CommMsgRcvd "com1" Console Text Text () TaskQ) xs) => 
    STrans q m sp xs '(xs, '[]) IDFunc Text
getConsoleMsg  = 
  let getIncomingConsoleMsg :: CommMsgRcvd "com1" Console Text Text () TaskQ -> Text 
      getIncomingConsoleMsg = getIncomingMsg
  in gets #com1 getIncomingConsoleMsg

instance (Has (CommMsgRcvd "com1" Console Text Text () TaskQ) xs) =>
    TransDict (ContT Bool) TaskQ ProxyAppDict "rcvd1ToSend2" xs (SendMsg Text) where
  getTransFromDict dict _ =  MkApp $ do
    txt <- getConsoleMsg
    return $ SendMsg txt 

instance (Has (CommMsgRcvd "com1" Console Text Text () TaskQ) xs) =>
  TransDict (ContT Bool) TaskQ ProxyAppDict "shouldQuit" xs Bool where
  getTransFromDict dict _ =  MkApp $ do
    txt <- getConsoleMsg
    return (txt == "q") 

transformWSMsg :: 
    (Has (CommMsgRcvd "com2" WSClient LBStr.ByteString Text () TaskQ) xs) => 
      STrans (ContT Bool) m sp xs '(xs, '[]) _ Text
transformWSMsg  = 
    let getIncomingConsoleMsg :: CommMsgRcvd "com2" WSClient LBStr.ByteString Text () TaskQ -> LBStr.ByteString 
        getIncomingConsoleMsg = getIncomingMsg
    in do 
      bstr <- gets #com2 getIncomingConsoleMsg
      return $ show bstr

instance (Has (CommMsgRcvd "com2" WSClient LBStr.ByteString Text () TaskQ) xs) =>
  TransDict (ContT Bool) TaskQ ProxyAppDict "rcvd2ToSend1" xs (SendMsg Text) where
  getTransFromDict dict _ =  MkApp $ do
    txt <- transformWSMsg
    return $ SendMsg txt 

type ConsoleWsApp = ProxyApp Console WSClient Text LBStr.ByteString Text Text () () TaskQ

{-
executableConsoleWsTrans :: ExcecutableTrans (ContT Bool) TaskQ ConsoleWsApp 
executableConsoleWsTrans = buildTrans ProxyAppDict
  
runConsoleWs :: IO ()
runConsoleWs = runAsyncTrans executableConsoleWsTrans
-}

-- to consider implementing dictionary items using the record below
data ProxyAppDict2 commPars1 commPars2 i1 i2 o1 o2 e1 e2 = 
  ProxyAppDict2 
    { commRes12 :: commPars1
    , commRes22 :: commPars2
    , i1o2 :: i1 -> o2
    , i2o1 :: i2 -> o1
    }

