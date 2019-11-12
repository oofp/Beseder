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

module  TimerForeverData where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Base
import           Beseder.Base.ControlData
import           Beseder.Base.Common (TaskPoster, Named)
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

{- type application fot type level string??
startTimer :: TaskPoster m  => Named timerName -> Int ->
    STransData m NoSplitter 
      (ComposeFunc
        (NewResFunc TimerRes timerName m)
        (InvokeAllFunc StartTimer "t1")) ()
startTimer timer timeoutSec  = do
  newRes timer TimerRes
  invoke timer (StartTimer timeoutSec)  
-}


timer5Pump :: TaskPoster m  => Int -> STransData m NoSplitter _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
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
  pumpEvents
  clearAllResources 

--runHandle :: IO ()
--runHandle =  runAsyncData $ timer5Pump 5   

-- :t evalSTransData (timer5Pump 1)

{-
!!! Renamer/typechecker [TimerForeverData]: finished in 123109.38 milliseconds, allocated 34619.883 megabytes
*** Desugar [TimerForeverData]:
Result size of Desugar (before optimization)
  = {terms: 12,811,
     types: 5,605,996,
     coercions: 134,895,926,
     joins: 0/56}
-}

timer5Handle :: TaskPoster m  => Int -> STransData m NoSplitter _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timer5Handle timeoutSec1 = do
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
  handleEvents $ do
    on @("t3" :? IsTimerTriggered :&& "t4" :? IsTimerArmed) (invoke #t4 StopTimer)
    on @("t5" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) (invoke #t2 StopTimer)
    on @("t2" :? IsTimerTriggered :&& "t5" :? IsTimerArmed) (invoke #t5 StopTimer)
  clear #t1 
  clear #t2 
  clear #t3 
  clear #t4 
  clear #t5 

--runHandle :: IO ()
--runHandle =  runAsyncData $ timer5Handle 5   

-- :t evalSTransData (timer5Handle 1)

