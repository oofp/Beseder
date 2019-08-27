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

module  SelfClosingDoor where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Resources.Composite
import           Data.String 
import           PureDoor 

type InitDoor m =
  NewRes "door" DoorRes m  
  :>> StartClosedTimer m

type OpenDoorReq m =
  StopClosedTimer
  :>> Invoke "door" OpenDoor
  :>> NewRes "openTimer" TimerRes m   
  :>> "openTimeout" |> Invoke "openTimer" StartTimer 
type OpenDoorRq = CrReqF TaskQ (OpenDoorReq TaskQ)

type CloseDoorReq m =
  Invoke "openTimer" StopTimer
  :>> ClearResource "openTimer"
  :>> Invoke "door" OpenDoor
  :>> StartClosedTimer m
type CloseDoorRq = CrReqF TaskQ (CloseDoorReq TaskQ)
  
type LockDoorReq =
  StopClosedTimer
  :>> Invoke "door" LockDoor
type LockDoorRq = CrReqF TaskQ LockDoorReq 

type UnlockDoorReq m =
  Invoke "door" UnlockDoor
  :>> StartClosedTimer m    
type UnlockDoorRq = CrReqF TaskQ (UnlockDoorReq TaskQ)


type StartClosedTimer m =  
  NewRes "closedTimer" TimerRes m   
  :>> "closedTimeout" |> Invoke "closedTimer" StartTimer 

type StopClosedTimer =  
  Invoke "closedTimer" StopTimer
  :>> ClearResource "closedTimer"


type DoorHandler m =
  On ("closedTimer" :? IsTimerTriggered)  
        ( ClearResource "closedTimer"
         :>> Invoke "door" LockDoor
         :>> PutStrLn "ClosedTimer triggered; door locked"
        )
  :>> On ("openTimer" :? IsTimerTriggered)  
        ( ClearResource "openTimer"
         :>> Invoke "door" CloseDoor
         :>> StartClosedTimer m
         :>> PutStrLn "OpenTimer triggered; door closed"
        )

doorDict openTimeout closedTimeout 
  = Patches  
    ( CnP (StartTimer openTimeout) `as` #openTimeout,
    ( CnP (StartTimer closedTimeout) `as` #closedTimeout))
      
selfClosingDoorRes :: (Int,Int) -> CrResF TaskQ (InitDoor TaskQ) (DoorHandler TaskQ) _
selfClosingDoorRes (openTimeout, closedTimeout) = 
  CrResF (doorDict openTimeout closedTimeout) 

type InitState = First (Eval (InitDoor TaskQ NoSplitter '[()])) 
type CloseTimerTriggeredState =First (Eval (GetNextAllFunc NoSplitter InitState))
type LockedState =First (Eval (DoorHandler TaskQ NoSplitter CloseTimerTriggeredState))
type UnlockResult_ShouldBeClosed = First (Eval (UnlockDoorReq TaskQ NoSplitter LockedState))
type OpenResult = First (Eval (OpenDoorReq TaskQ NoSplitter UnlockResult_ShouldBeClosed))
type OpenTimerTriggeredState =First (Eval (GetNextAllFunc NoSplitter OpenResult))
type ClosedStateByHandling = First (Eval (DoorHandler TaskQ NoSplitter OpenTimerTriggeredState))
type LockResult = First (Eval (LockDoorReq NoSplitter ClosedStateByHandling))

-- :kind! EvalTransFunc TaskQ InitDoor  