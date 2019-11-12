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

module  TimerIxApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           qualified Beseder.Base.Control as C                                               
import           Beseder.Base.Base
import           Beseder.Base.Control
import           Beseder.Base.Common (TaskPoster)
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           qualified Protolude 
import           Beseder.Base.Internal.STransIxDo
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.NatOne

timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHello timeoutSec1 = do
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  nextEv'
  clear #t1 

  {-
timer5 :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5 timeoutSec1 = do
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
  nextEv'
  nextEv'
  nextEv'
  nextEv'
  nextEv'
  clear #t1 
  clear #t2 
  clear #t3 
  clear #t4 
  clear #t5 
-}
  {- for 4 timers
  !!! Renamer/typechecker [TimerIxApps]: finished in 578.12 milliseconds, allocated 406.584 megabytes
  *** Desugar [TimerIxApps]:
  Result size of Desugar (before optimization)
    = {terms: 3,424, types: 93,411, coercions: 594,448, joins: 0/1,111}  
  
- for 5 timers    
  !!! Renamer/typechecker [TimerIxApps]: finished in 3187.50 milliseconds, allocated 2246.153 megabytes
*** Desugar [TimerIxApps]:
Result size of Desugar (before optimization)
  = {terms: 6,024,
     types: 339,875,
     coercions: 4,696,050,
     joins: 0/1,945}
-}  

{-
timer5Try :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5Try timeoutSec1 = do
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
  try @C.Dynamics $ try @("t5" C.:? IsTimerArmed) $ do 
    nextEv
    nextEv
    nextEv
    nextEv
    nextEv
-}

{-    
nextEv'
!!! Renamer/typechecker [TimerIxApps]: finished in 2203.12 milliseconds, allocated 1552.151 megabytes
*** Desugar [TimerIxApps]:
Result size of Desugar (before optimization)
  = {terms: 4,827,
      types: 560,536,
      coercions: 2,747,210,
      joins: 0/1,606}

nextEv'      
!!! Renamer/typechecker [TimerIxApps]: finished in 2187.50 milliseconds, allocated 1610.292 megabytes
*** Desugar [TimerIxApps]:
Result size of Desugar (before optimization)
  = {terms: 5,114,
     types: 593,366,
     coercions: 2,801,956,
     joins: 0/1,705}      
-}

{-
timer5Pump :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5Pump timeoutSec1 = do
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
  try @("t5" C.:? IsTimerArmed)  pumpEvents 
-}

timer5Handle :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5Handle timeoutSec1 = do
  --newRes #t1 TimerRes 
  --invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)  
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1)  
  newRes #t4 TimerRes 
  invoke #t4  (StartTimer timeoutSec1)  
  newRes #t5 TimerRes 
  invoke #t5  (StartTimer timeoutSec1)
  handleEvents $ do
    on @("t3" C.:? IsTimerTriggered C.:&& "t4" C.:? IsTimerArmed) (invoke #t4 StopTimer)
    on @("t5" C.:? IsTimerTriggered C.:&& "t2" C.:? IsTimerArmed) (invoke #t2 StopTimer)
    on @("t2" C.:? IsTimerTriggered C.:&& "t5" C.:? IsTimerArmed) (invoke #t5 StopTimer)
  -- clear #t1 
  clear #t2 
  clear #t3 
  clear #t4 
  clear #t5 

runTest :: IO ()
runTest =  runAsyncTrans $ timer5Handle 5   

{-
timer5Skip :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5Skip timeoutSec1 = do
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
  --nextSteps (Proxy @One)
  --try @(C.Dynamics) 
  skipAll
  clear #t1 
  clear #t2 
  clear #t3 
  clear #t4 
  clear #t5 

runTest :: IO ()
runTest =  runSTrans $ timer5Skip 5   
-}

timer5SkipTo :: TaskPoster m  => Int -> STrans (ContT Bool) m C.NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5SkipTo timeoutSec1 = do
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
  skipTo @("t1" C.:? IsTimerTriggered)

runHello :: IO ()
runHello = runAsyncTrans (timerHello 5)

