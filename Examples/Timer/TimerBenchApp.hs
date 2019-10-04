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
-- {-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerBenchApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, wait)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           qualified Protolude 

{-
timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHello timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)  
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)  
  nextEv 
  nextEv 
  nextEv 
  clearAllResources 
-- 1) = {terms: 404, types: 6,414, coercions: 15,825, joins: 0/99}  
-- 2) = {terms: 817, types: 16,191, coercions: 104,899, joins: 0/201}
-}

{-
timerHelloPump :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHelloPump timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  try @("t1" :? IsTimerArmed :|| "t2" :? IsTimerArmed)  
    pumpEvents
  --clear #t1 
  clearAllResources 

-- 1) = {terms: 502, types: 9,162, coercions: 148,214, joins: 0/125}
-- 2) = {terms: 840, types: 19,347, coercions: 2,028,882, joins: 0/207}
-}
{-
timerHelloWait :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHelloWait timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)
  --on @("t1" :? IsTimerArmed :|| "t2" :? IsTimerArmed)  
  -- try @("t1" :? IsTimerArmed :|| "t2" :? IsTimerArmed :|| "t3" :? IsTimerArmed)  
  skipTo @("t1" :? IsTimerTriggered)  
  --  pumpEvents
  clear #t1 
  clear #t2
  clear #t3
  clear #t4
  --clearAllResources 
-- 2) =  {terms: 842, types: 18,368, coercions: 431,552, joins: 0/209}  
-- 3) =  {terms: 1,479, types: 45,012, coercions: 3,596,379, joins: 0/361}
-- = {terms: 1,479, types: 45,012, coercions: 3,596,379, joins: 0/361}
-- 2a) = {terms: 866, types: 18,997, coercions: 446,300, joins: 0/215}
-- 4/1 try+wait = {terms: 3,099, types: 166,245, coercions: 10,570,835, joins: 0/747}
-- 4/1 try+ pump = {terms: 3,018, types: 208,249, coercions: 93,628,288, joins: 0/715}
-- 4/1 skip   = {terms: 3,077, types: 161,116, coercions: 11,772,270,joins: 0/739}
-}


--timerHelloApp :: TaskPoster m  => Int -> STransApp (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
--timerHelloApp = MkApp . timerHello

-- runAsyncTrans $ twoTimersOn 4 7  
-- runAsyncTrans (instrumentTrans varIndexInstr  (twoTimersOn 5 3))
-- runAsyncTrans (instrumentTrans varLengthInstr  (twoTimersOn 5 3))

{-
type Timers3 m =
  '[  (TimerArmed m "t", (TimerArmed m "t1", TimerArmed m "t3")) 
   ,  (TimerTriggered m "t", (TimerArmed m "t1", TimerArmed m "t3")) 
   ,  (TimerTriggered m "t", (TimerTriggered m "t1", TimerArmed m "t3")) 
   ,  (TimerArmed m "t", (TimerTriggered m "t1", TimerArmed m "t3")) 
   ,  (TimerArmed m "t", (TimerTriggered m "t1", TimerTriggered m "t3")) 
   ,  (TimerArmed m "t", (TimerArmed m "t1", TimerTriggered m "t3")) 
   ]
 
timer3 :: STrans (ContT Bool) TaskQ NoSplitter (Timers3 TaskQ) _ _ ()
timer3 = nextEv
-- = {terms: 432, types: 21,020, coercions: 305,498, joins: 0/0}
-- = {terms: 1,211, types: 25,740, coercions: 300,401, joins: 0/1}

apply3 = applyTransApp (MkApp timer3)
-- = {terms: 438, types: 21,490, coercions: 311,172, joins: 0/0}
-}

{-
timer4_0 :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer4_0 timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)
  nextEv
  nextEv
  nextEv
  nextEv
  clearAllResources
-}  
--   = {terms: 1,971, types: 80,155, coercions: 3,177,788, joins: 0/457}
-- !!! Desugar [TimerBenchApp]: finished in 2734.38 milliseconds, allocated 2912.210 megabytes

{-
timer4_0pumps :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer4_0pumps timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)
  pumpEvents
  clearAllResources
-}

{-
!!! Renamer/typechecker [TimerBenchApp]: finished in 18546.88 milliseconds, allocated 18427.366 megabytes
*** Desugar [TimerBenchApp]:
Result size of Desugar (before optimization)
  = {terms: 3,405,
     types: 283,711,
     coercions: 23,626,676,
     joins: 0/1,101}
-}  
{-
timer5_0 :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5_0 timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)
  newRes #t5 TimerRes 
  invoke #t5  (StartTimer timeoutSec1)
  nextEv
  nextEv
  nextEv
  nextEv
  nextEv
  clearAllResources
-}

{-
!!! Renamer/typechecker [TimerBenchApp]: finished in 38937.50 milliseconds, allocated 21288.199 megabytes
*** Desugar [TimerBenchApp]:
Result size of Desugar (before optimization)
  = {terms: 5,494,
     types: 400,424,
     coercions: 22,434,675,
     joins: 0/1,747}
-}

timer5_0wait :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '[()] '[] _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5_0wait timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)
  newRes #t5 TimerRes 
  invoke #t5  (StartTimer timeoutSec1)
  try @Dynamics skipAll
  clearAllResources
  
{-  
  !!! Renamer/typechecker [TimerBenchApp]: finished in 48906.25 milliseconds, allocated 52075.597 megabytes
  *** Desugar [TimerBenchApp]:
  Result size of Desugar (before optimization)
    = {terms: 5,604,
       types: 392,607,
       coercions: 60,591,249,
       joins: 0/1,792}
-}
       
{-
timer4_1 :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer4_1 timeoutSec1 = 
  (((((((((((((liftIO $ putStrLn ("Entered timerHello"::Text)) >> 
  newRes #t1 TimerRes) >>
  invoke #t1  (StartTimer timeoutSec1)) >>
  newRes #t2 TimerRes) >>  
  invoke #t2  (StartTimer timeoutSec1)) >>
  newRes #t3 TimerRes) >> 
  invoke #t3  (StartTimer timeoutSec1)) >> 
  newRes #t4 TimerRes) >> 
  invoke #t4  (StartTimer timeoutSec1)) >>
  nextEv) >>
  nextEv) >>
  nextEv) >>
  nextEv) >>
  clearAllResources
-- {terms: 1,971, types: 81,813, coercions: 1,604,228, joins: 0/457} 
-- !!! Desugar [TimerBenchApp]: finished in 1390.62 milliseconds, allocated 1530.610 megabytes
-}

{-
timer4_1pump :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer4_1pump timeoutSec1 = 
  ((((((((((liftIO $ putStrLn ("Entered timerHello"::Text)) >> 
  newRes #t1 TimerRes) >>
  invoke #t1  (StartTimer timeoutSec1)) >>
  newRes #t2 TimerRes) >>  
  invoke #t2  (StartTimer timeoutSec1)) >>
  newRes #t3 TimerRes) >> 
  invoke #t3  (StartTimer timeoutSec1)) >> 
  newRes #t4 TimerRes) >> 
  invoke #t4  (StartTimer timeoutSec1)) >>
  pumpEvents) >>
  clearAllResources
-}

{-
!!! Renamer/typechecker [TimerBenchApp]: finished in 8968.75 milliseconds, allocated 6197.246 megabytes
*** Desugar [TimerBenchApp]:
Result size of Desugar (before optimization)
  = {terms: 3,405,
     types: 273,135,
     coercions: 11,876,496,
     joins: 0/1,101}
-}  

{-
timer5_1pump :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5_1pump timeoutSec1 = 
  ((((((((((((liftIO $ putStrLn ("Entered timerHello"::Text)) >> 
  newRes #t1 TimerRes) >>
  invoke #t1  (StartTimer timeoutSec1)) >>
  newRes #t2 TimerRes) >>  
  invoke #t2  (StartTimer timeoutSec1)) >>
  newRes #t3 TimerRes) >> 
  invoke #t3  (StartTimer timeoutSec1)) >> 
  newRes #t4 TimerRes) >> 
  invoke #t4  (StartTimer timeoutSec1)) >>
  newRes #t5 TimerRes) >> 
  invoke #t5  (StartTimer timeoutSec1)) >>
  pumpEvents) >>
  clearAllResources
-}
  {-
!!! Renamer/typechecker [TimerBenchApp]: finished in 79406.25 milliseconds, allocated 48379.724 megabytes
*** Desugar [TimerBenchApp]:
Result size of Desugar (before optimization)
  = {terms: 7,235,
     types: 1,553,602,
     coercions: 112,561,733,
     joins: 0/2,352}
  !!! Renamer/typechecker [TimerBenchApp]: finished in 54203.12 milliseconds, allocated 48393.819 megabytes
  *** Desugar [TimerBenchApp]:
  Result size of Desugar (before optimization)
    = {terms: 7,235,
       types: 1,553,978,
       coercions: 112,578,890,
       joins: 0/2,352}
  -}

{-  
timer5_1wait :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5_1wait timeoutSec1 = 
  ((((((((((((liftIO $ putStrLn ("Entered timerHello"::Text)) >> 
  newRes #t1 TimerRes) >>
  invoke #t1  (StartTimer timeoutSec1)) >>
  newRes #t2 TimerRes) >>  
  invoke #t2  (StartTimer timeoutSec1)) >>
  newRes #t3 TimerRes) >> 
  invoke #t3  (StartTimer timeoutSec1)) >> 
  newRes #t4 TimerRes) >> 
  invoke #t4  (StartTimer timeoutSec1)) >>
  newRes #t5 TimerRes) >> 
  invoke #t5  (StartTimer timeoutSec1)) >>
  (try @Dynamics wait)) >>
  clearAllResources
-}
  
{-
!!! Renamer/typechecker [TimerBenchApp]: finished in 15484.38 milliseconds, allocated 15431.132 megabytes
*** Desugar [TimerBenchApp]:
Result size of Desugar (before optimization)
  = {terms: 5,604,
     types: 392,855,
     coercions: 28,517,939,
     joins: 0/1,792}
-}
  
type Timers5 m =
  '[  (TimerArmed m "t1", (TimerArmed m "t2", (TimerArmed m "t3", (TimerArmed m "t4",TimerArmed m "t5")))) ] 
  