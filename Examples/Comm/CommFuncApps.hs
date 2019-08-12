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
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  CommApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Control.Monad.Cont (ContT)                                               
import           Beseder.Base.Control                                               
import           Beseder.Resources.Comm 
import           Beseder.Resources.State.DataRes
import           Data.String 

type CompletedRes = '(('[()]),'[])

-- :kind! EvalTransFunc IO (ProxyApp Int Int Int Int Int Int Int Int)
-- :kind! EvalTransFuncWithTrace IO (ProxyApp Int Int Int Int Int Int Int Int)

type ProxyApp commPars1 commPars2 i1 i2 o1 o2 e1 e2 m =
  DictFunc "comRes1" :>>= NewRes "com1" (CommRes commPars1 i1 o1 e1) m
  :>> DictFunc "comRes2" :>>= NewRes "com2" (CommRes commPars1 i1 o1 e1) m
  :>> NewRes "flCont" InitAsTrue m
  :>> Trace "comm created"
  :>> Try(("com1" :? IsCommAlive) :&& ("com2" :? IsCommAlive) :&& ("flCont" :? IsTrue)) 
    ( HandleEvents
      ( On ("com1" :? IsMessageReceived)
            ( On ("com2" :? IsCommConnected) 
                ( Trace "com2Connected"
                :>> DictFunc "rcvd1ToSend2" :>>= Invoke "com2" (SendMsg o2) 
                )
              :>> Invoke "com1" GetNextMsg
            )  
      :>> On ("com2" :? IsMessageReceived)
          ( On ("com1" :? IsCommConnected) 
              ( Trace "com1Connected"
              :>> DictFunc "rcvd2ToSend1" :>>= Invoke "com1" (SendMsg o1) 
              )
            :>> Invoke "com2" GetNextMsg
          )
      )
    )
    :>> On ("com1" :? IsCommAlive) (Invoke "com1" CloseComm)
    :>> On ("com2" :? IsCommAlive) (Invoke "com2" CloseComm)
    :>> ClearAllResourcesButTrace
    