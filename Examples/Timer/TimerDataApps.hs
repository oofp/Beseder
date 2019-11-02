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
import qualified Beseder.Base.Internal.STransDataDo as D
import           qualified Protolude 

{-
timerHelloData :: Int -> STransData m NoSplitter _ _ _ _ () 
timerHelloData timeoutSec1 = do
  Compose 
    (NewRes #t1 TimerRes) 
    (Compose 
      (Invoke #t1  (StartTimer timeoutSec1))  
      (Compose 
        NextEv' 
        (Clear #t1))) 
-}

timerHelloData :: Int -> STransData m NoSplitter _ _ _ _ () 
timerHelloData timeoutSec1 = do
   D.newRes #t1 TimerRes
   D.invoke #t1  (StartTimer timeoutSec1)
   D.nextEv 
   D.clear #t1


timerHello :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello timeoutSec1 = interpret (timerHelloData timeoutSec1)

{-
timerData4 :: forall m. Int -> STransData m NoSplitter _ _ _ _ () 
timerData4 timeoutSec1 = 
  NewRes #t1 TimerRes
  >:> NewRes #t2 TimerRes
  >:> NewRes #t3 TimerRes
  >:> NewRes #t4 TimerRes
  >:> (Return timeoutSec1 >*> (Invoke #t1 . StartTimer))
  >:> Invoke #t2  (StartTimer timeoutSec1)
  >:> Invoke #t3  (StartTimer timeoutSec1)
  >:> Invoke #t4  (StartTimer timeoutSec1)
  -- >:> On @(By (TimerArmed m "t1")) --Try 
  >:> Try @("t1" :? IsTimerArmed)
        ( NextEv' 
          >:> NextEv' 
          >:> NextEv' 
          >:> NextEv' 
        )
  >:> On @("t1" :? IsTimerTriggered)
    ( Clear #t1
      >:> Clear #t2
      >:> Clear #t3
      >:> Clear #t4
    )
-}

timerData4 :: forall m. Int -> STransData m NoSplitter _ _ _ _ () 
timerData4 timeoutSec1 = do 
  NewRes #t1 TimerRes
  NewRes #t2 TimerRes
  NewRes #t3 TimerRes
  NewRes #t4 TimerRes
  (Return timeoutSec1) >>=  (Invoke #t1 . StartTimer)
  Invoke #t2  (StartTimer timeoutSec1)
  Invoke #t3  (StartTimer timeoutSec1)
  Invoke #t4  (StartTimer timeoutSec1)
  -- >:> On @(By (TimerArmed m "t1")) --Try 
  Try @("t1" :? IsTimerArmed) $ do
    NextEv' 
    NextEv' 
    NextEv' 
    NextEv' 
  On @("t1" :? IsTimerTriggered) $ do
    Clear #t1
    Clear #t2
    Clear #t3
    Clear #t4

timer4 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timer4 = interpret . timerData4 


timerData4a :: forall m. Int -> STransData m NoSplitter _ _ _ _ () 
timerData4a timeoutSec1 = do 
  NewRes #t1 TimerRes
  Invoke #t1 (StartTimer timeoutSec1)
  Try @("t1" :? IsTimerArmed) NextEv'



timerVarData :: forall t1 t2 m. (_) => Named t1 -> Named t2 -> Int -> STransData m NoSplitter _ _ _ _ () 
timerVarData t1n t2n timeoutSec1 = do 
  --NewRes t1n TimerRes   
  --NewRes t2n TimerRes
  Invoke t1n  (StartTimer timeoutSec1)
  Invoke t2n  (StartTimer timeoutSec1)
  --NextEv' 
  --NextSteps (Proxy @One) 
  --Try @(Dynamics)
  --  (NextSteps (Proxy @(Succ One)))
  --NextEv'
  D.skipAll
  Clear t1n
  Clear t2n

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
  
  
timerDataSkipTo :: forall m. (_) => Int -> STransData m NoSplitter _ _ _ _ () 
timerDataSkipTo timeoutSec1 = do 
  NewRes #t1 TimerRes
  NewRes #t2 TimerRes
  NewRes #t3 TimerRes
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
