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

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  TimerDataApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import qualified Beseder.Base.ControlData as D
import           qualified Protolude 

{-
timerHelloData :: Int -> STransData m NoSplitter _ _ _ _ () 
timerHelloData timeoutSec1 = do
  Compose 
    (D.newRes #t1 TimerRes) 
    (Compose 
      (D.invoke #t1  (StartTimer timeoutSec1))  
      (Compose 
        D.nextEv' 
        (D.clear #t1))) 
-}

timerHelloData :: Int -> D.STransData m NoSplitter _ _ _ _ () 
timerHelloData timeoutSec1 = do
   D.newRes #t1 TimerRes
   D.invoke #t1  (StartTimer timeoutSec1)
   D.nextEv 
   D.clear #t1


timerHello :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello timeoutSec1 = interpret (timerHelloData timeoutSec1)


timerData4 :: forall m. Int -> D.STransData m NoSplitter _ _ _ _ () 
timerData4 timeoutSec1 = do 
  D.newRes #t1 TimerRes
  D.newRes #t2 TimerRes
  D.newRes #t3 TimerRes
  D.newRes #t4 TimerRes
  (return timeoutSec1) >>=  (D.invoke #t1 . StartTimer)
  D.invoke #t2  (StartTimer timeoutSec1)
  D.invoke #t3  (StartTimer timeoutSec1)
  D.invoke #t4  (StartTimer timeoutSec1)
  -- >:> On @(By (TimerArmed m "t1")) --Try 
  D.try @("t1" :? IsTimerArmed) $ do
    D.nextEv' 
    D.nextEv' 
    D.nextEv' 
    D.nextEv' 
  D.on @("t1" :? IsTimerTriggered) $ do
    D.clear #t1
    D.clear #t2
    D.clear #t3
    D.clear #t4

timer4 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timer4 = interpret . timerData4 


timerData4a :: forall m. Int -> D.STransData m NoSplitter _ _ _ _ () 
timerData4a timeoutSec1 = do 
  D.newRes #t1 TimerRes
  D.invoke #t1 (StartTimer timeoutSec1)
  D.try @("t1" :? IsTimerArmed) D.nextEv'



timerVarData :: forall t1 t2 m. (_) => Named t1 -> Named t2 -> Int -> D.STransData m NoSplitter _ _ _ _ () 
timerVarData t1n t2n timeoutSec1 = do 
  --D.newRes t1n TimerRes   
  --D.newRes t2n TimerRes
  D.invoke t1n  (StartTimer timeoutSec1)
  D.invoke t2n  (StartTimer timeoutSec1)
  --D.nextEv' 
  --NextSteps (Proxy @One) 
  --Try @(Dynamics)
  --  (NextSteps (Proxy @(Succ One)))
  --D.nextEv'
  D.skipAll
  D.clear t1n
  D.clear t2n

timerVar :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerVar timeoutSec1 = do
  newRes #t1 TimerRes
  newRes #t2 TimerRes
  (interpret (timerVarData #t1 #t2 timeoutSec1)) 

timerVar2 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerVar2 timeoutSec1 = do
  newRes #t1 TimerRes
  newRes #t2 TimerRes
  newRes #t3 TimerRes
  invoke #t3 (StartTimer 1)
  (interpret (timerVarData #t1 #t2 timeoutSec1)) 
  clear #t3
  
  
timerDataSkipTo :: forall m. (_) => Int -> D.STransData m NoSplitter _ _ _ _ () 
timerDataSkipTo timeoutSec1 = do 
  D.newRes #t1 TimerRes
  D.newRes #t2 TimerRes
  D.newRes #t3 TimerRes
  D.invoke #t1 (StartTimer timeoutSec1)  
  D.invoke #t2 (StartTimer timeoutSec1)  
  D.invoke #t3 (StartTimer timeoutSec1)
  D.try @Dynamics $
    D.skipTo @("t2" :? IsTimerTriggered)


timerSkipTo :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerSkipTo timeoutSec1 = interpret $ timerDataSkipTo timeoutSec1

timerSkipTo2 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerSkipTo2 timeoutSec1 = do
  newRes #t4 TimerRes
  invoke #t4 (StartTimer timeoutSec1)  
  interpret $ timerDataSkipTo timeoutSec1
