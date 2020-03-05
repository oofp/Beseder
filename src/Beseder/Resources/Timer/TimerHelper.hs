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
{-# LANGUAGE OverloadedLabels       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.Resources.Timer.TimerHelper
  ( startTimer
  , delay
  , withTimer
  , withTimeLimit
  , handleWithTimer
  , handleWithTimeLimit
  , pumpWithTimer
  , skipWithTimeLimitTo
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Common
import           Beseder.Base.ControlData
import           Beseder.Resources.Timer.TimerRes hiding (startTimer) 

startTimer :: forall name m sp. Named name -> Int -> 
                STransData m sp (ComposeFunc (NewResFunc TimerRes name m) (InvokeAllFunc StartTimer name)) ()
startTimer timerName timeoutSec = do
  newRes timerName TimerRes 
  invoke timerName (StartTimer timeoutSec)

withTimeLimit :: forall name m sp f. Named name -> Int -> STransData m (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp _ ()
withTimeLimit timerName timeoutSec sub = do
  startTimer timerName timeoutSec
  try @(name :? IsTimerArmed) sub
  clear timerName

withTimer :: forall name m sp f f_c. Named name -> Int -> 
                  STransData m sp f_c () ->
                  STransData m (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp _ () 
withTimer timerName timeoutSec c_sub sub = scopeRes $ do
  startTimer timerName timeoutSec
  try @(name :? IsTimerArmed) sub
  on @(name :? IsTimerTriggered) c_sub
  clear timerName


handleWithTimer :: forall name m sp f f_c. Named name -> Int -> 
                  STransData m sp f_c () ->
                  STransData m ((sp :&& (name :? IsTimerArmed)) :&& Dynamics) f () -> -- (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp _ ()
handleWithTimer timerName timeoutSec c_sub h_sub = scopeRes $ do
  newRes timerName TimerRes 
  invoke timerName (StartTimer timeoutSec)
  try @(name :? IsTimerArmed) (handleEvents h_sub)
  on @(name :? IsTimerTriggered) c_sub
  clear timerName

handleWithTimeLimit :: forall name m sp f. Named name -> Int -> 
                  STransData m ((sp :&& (name :? IsTimerArmed)) :&& Dynamics) f () -> -- (sp :&& name :? IsTimerArmed) f () -> 
                  STransData m sp _ ()
handleWithTimeLimit timerName timeoutSec h_sub = scopeRes $ do
  newRes timerName TimerRes 
  invoke timerName (StartTimer timeoutSec)
  try @(name :? IsTimerArmed) (handleEvents h_sub)
  clear timerName

pumpWithTimer :: forall name m sp f_c. Named name -> Int -> 
                  STransData m sp f_c () ->
                  STransData m sp _ ()
pumpWithTimer timerName timeoutSec c_sub = scopeRes $ do
  startTimer timerName timeoutSec 
  try @(name :? IsTimerArmed) pumpEvents
  on @(name :? IsTimerTriggered) c_sub
  clear timerName

delay :: forall name m sp f. Named name -> Int -> STransData m sp f () ->
                  STransData m sp _ ()
delay timerName timeoutSec sub = do
  sub
  startTimer timerName timeoutSec 
  try @(name :? IsTimerArmed) pumpEvents
  clear timerName

skipWithTimeLimitTo' :: forall sp1 name m sp. Proxy sp1 ->Named name -> Int -> 
                  STransData m sp _ ()
skipWithTimeLimitTo' _px timerName timeoutSec  = do
  startTimer timerName timeoutSec 
  try @(name :? IsTimerArmed) (skipTo @sp1)
  clear timerName

skipWithTimeLimitTo :: forall sp1 name m sp. Named name -> Int -> 
                  STransData m sp _ ()
skipWithTimeLimitTo = skipWithTimeLimitTo' (Proxy @sp1)


-- ghcid --command "stack ghci ./src/Beseder/Resources/Timer/TimerHelper.hs"