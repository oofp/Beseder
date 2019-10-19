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
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  TwoTimersCompApp where 

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Resources.Timer
import           Beseder.Resources.Composite
import           Control.Monad.Identity (IdentityT)
import           Control.Monad.Cont (ContT)
import           TwoTimersComp
import           Beseder.Misc.Misc
--import           qualified Protolude 

twoTimersApp :: TaskPoster m  => Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] _ _ ()
twoTimersApp timeoutSec1 timeoutSec2 = MkApp $ do
  newTwoTimers #tt timeoutSec1 timeoutSec2
  nextEv
  clear #tt

twoTimersApp2 :: TaskPoster m  => Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] _ _ ()
twoTimersApp2 timeoutSec1 timeoutSec2 = MkApp $ do
  newRes #t3 TimerRes
  newTwoTimers #tt timeoutSec1 timeoutSec2
  invoke #t3 (StartTimer timeoutSec2)
  nextEv
  on @("t3" :? IsTimerTriggered) (invokeStopTwoTimers #tt)
  on @("t3" :? IsTimerArmed) (invoke #t3 StopTimer)
  clearAllResources

twoTimersApp3 :: TaskPoster m  => Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] _ _ ()
twoTimersApp3 timeoutSec1 timeoutSec2 = MkApp $ do
  newRes #t3 TimerRes
  newTwoTimers #tt timeoutSec1 timeoutSec2
  invoke #t3 (StartTimer timeoutSec2)
  skipAll
  clearAllResources

twoTimersApp4 :: TaskPoster m  => Int -> Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] _ _ ()
twoTimersApp4 timeoutSec1 timeoutSec2 timeoutSec3 = MkApp $ do
  newRes #t3 TimerRes
  newTwoTimers #tt1 timeoutSec1 timeoutSec2
  newTwoTimers #tt2 timeoutSec1 timeoutSec2
  invoke #t3 (StartTimer timeoutSec3)
  skipTo @("t3" :? IsTimerTriggered)
  on @("tt1" :? IsTwoTimersAlive) $ do 
    _p :: _ <- whatNext
    invokeStopTwoTimers #tt1 
  on @("tt2" :? IsTwoTimersAlive) (invokeStopTwoTimers #tt2)
  clearAllResources
-- runAsyncApp (twoTimersApp4 5 4 2)