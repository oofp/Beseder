{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  ThreeTimersComp 
  ( ThreeTimers (..)
  , StopAllTimers (..) 
  , ThreeTimersRes
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import           Beseder.Base.ControlData                                               
import           Beseder.Resources.State.DataRes
import           Beseder.Resources.Timer
import           Beseder.Resources.Composite

type ThreeTimersRes = CRdPar ThreeTimers
data ThreeTimers = ThreeTimers {to1::Int, to2 :: Int, to3 :: Int}

type InitCompFunc m =
    (ComposeFunc
    (NewResFunc TimerRes "t1" m)
    (ComposeFunc
       (NewResFunc TimerRes "t2" m)
       (ComposeFunc
          (InvokeAllFunc StartTimer "t1")
          (ComposeFunc
             (InvokeAllFunc StartTimer "t2")
             (NewResFunc (InitData Int) "cfg" m)))))

initComp :: ThreeTimers -> STransData m NoSplitter (InitCompFunc m) ()
initComp tt = do
  newRes #t1 TimerRes     
  newRes #t2 TimerRes  
  invoke #t1 (StartTimer (to1 tt))    
  invoke #t2 (StartTimer (to2 tt))
  newRes #cfg (InitData (to3 tt))
-- evalSTransData (initComp (ThreeTimers 1 1 1))   

type HandleCompFunc m = 
  (CaptureFunc
  ("t2" :? IsTimerTriggered)
  (ComposeFunc
     (ClearAllFunc "t2")
     (BindFunc
        (GetFunc "cfg" (StD Int "cfg"))
        (ComposeFunc
           (NewResFunc TimerRes "t3" m) (InvokeAllFunc StartTimer "t3")))))

           
handleComp :: STransData m NoSplitter (HandleCompFunc m) ()
handleComp = do
  on @("t2" :? IsTimerTriggered) $ do
    clear #t2
    tm3 <- gets #cfg getData
    newRes #t3 TimerRes  
    invoke #t3 (StartTimer tm3)    

instance CompositeDataRes m ThreeTimers where
  type CreateFunc m ThreeTimers = InitCompFunc m
  type HandlerFunc m ThreeTimers = HandleCompFunc m 
  resInit = initComp
  stateHandler _ = handleComp

handleStopReq :: STransData m NoSplitter StopAllTimersFunc ()
handleStopReq = do
  on @("t1" :? IsTimerArmed) $ do
    invoke #t1 StopTimer
  on @("t2" :? IsTimerArmed) $ do
    invoke #t2 StopTimer
  on @("t3" :? IsTimerArmed) $ do
    invoke #t3 StopTimer
    
data StopAllTimers = StopAllTimers deriving Show
type StopAllTimersFunc =
  (ComposeFunc
  (CaptureFunc ("t1" :? IsTimerArmed) (InvokeAllFunc StopTimer "t1"))
  (ComposeFunc
    (CaptureFunc ("t2" :? IsTimerArmed) (InvokeAllFunc StopTimer "t2"))
    (CaptureFunc
        ("t3" :? IsTimerArmed) (InvokeAllFunc StopTimer "t3"))))

instance CompositeDataReq m StopAllTimers where
  type RequestFunc m StopAllTimers = StopAllTimersFunc  
  type ReqSplitter StopAllTimers = ("t1" :? IsTimerArmed :|| "t2" :? IsTimerArmed :|| "t3" :? IsTimerArmed) 
  reqHandler _ = handleStopReq
