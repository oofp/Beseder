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
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module  EntranceDoor where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Resources.Monitor.BinaryMonitorRes
import           Beseder.Resources.State.ImpRes 
import           Beseder.Resources.State.DataRes 
import           Beseder.Resources.Comm 
import           Data.String 
import           qualified Protolude 

data FobReader = FobReader 

type InitState m 
    = '[  ( BinSwitchOff "door",
          ( BinMonitorOff m "inDet", 
          ( BinMonitorOff m "outDet",  
          ( CommWaitForMsg "fobReader" FobReader () () () m )))) 
       ]

type FobReaderAlive = "fobReader" :? IsCommAlive 

doorHandler :: forall sp s m. Int -> STransData m sp _ ()
doorHandler doorTimeoutSec = 
  handleEvents $ do
    on @("fobReader" :? IsMessageReceived) $ do 
      invoke #fobReader GetNextMsg
      openDoorIfClosed doorTimeoutSec       
    on @("inDet" :? IsBinMonitorOn) $ do 
      openDoorIfClosed doorTimeoutSec    
    on @("doorTimer" :? IsTimerTriggered) $ do 
      onOrElse @("inDet" :? IsBinMonitorOn :|| "outDet" :? IsBinMonitorOn)
        (restartTimer doorTimeoutSec)
        closeDoor        
    --assertion    
    assertCheck @InvalidConditions
    {-
    on @(Not (By "failure")) $ do  
      on @InvalidConditions $
        newRes #failure (InitData ())
    -}                  
openDoorIfClosed :: Int -> STransData m sp _ ()     
openDoorIfClosed doorTimeoutSec = do
  on @("door" :? IsBinSwitchOff) $ do
    invoke #door TurnOn
    newRes #doorTimer TimerRes
    invoke #doorTimer (StartTimer doorTimeoutSec)

closeDoor :: STransData m sp _ () 
closeDoor = do
  clear #doorTimer   
  invoke #door TurnOff

restartTimer :: Int -> STransData m sp _ () 
restartTimer doorTimeoutSec = do
  clear #doorTimer
  newRes #doorTimer TimerRes
  invoke #doorTimer (StartTimer doorTimeoutSec)

normalDoorHandler :: Int -> STransData m FobReaderAlive _ ()
normalDoorHandler =  doorHandler
  
assertDoorHandler :: Int -> STransData m FobReaderAlive _ ()
assertDoorHandler doorTimeoutSec = 
  try @(Not (By "failure")) $ do
    doorHandler doorTimeoutSec

evalDoorHandler = evalSTransData' (normalDoorHandler 5) (Proxy @(InitState IO))

type InvalidConditions =
  ("doorTimer" :? IsTimerTriggered 
  :|| ("door" :? IsBinSwitchOn :&& (Not ("doorTimer" :? IsTimerArmed))) 
  :|| ("door" :? IsBinSwitchOff :&& ("inDet" :? IsBinMonitorOn)) 
  :|| ("door" :? IsBinSwitchOff :&& ("outDet" :? IsBinMonitorOn)) --invalid 
  )

assertCheck :: forall cond m sp. STransData m sp _ ()     
assertCheck = do
  on @(Not (By "failure")) $ do  
    on @cond $
      newRes #failure (InitData ())
  
evalAssertDoorHandler = evalSTransData' (assertDoorHandler 5) (Proxy @(InitState IO))
