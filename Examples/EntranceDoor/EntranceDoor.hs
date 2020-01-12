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
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  EntranceDoor where

import           Protolude                    hiding (Product, Any, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Resources.Timer
import           Beseder.Resources.Monitor
import           Beseder.Resources.Switch
import           Beseder.Resources.Comm 
import           Data.String 
import           GHC.Exts (Any)    

data FobReader = FobReader 

type InitState m doorSw monRes
    = '[  ( StBinSwitchOff m doorSw "door",
          ( StBinMonitorOff m monRes "inDet", 
          ( StBinMonitorOff m monRes "outDet",  
          ( CommWaitForMsg "fobReader" FobReader () () () m )))) 
       ]

type FobReaderAlive = "fobReader" :? IsCommAlive 

doorHandlerCon :: forall sp m. Int -> STransData m sp _ ()
doorHandlerCon doorTimeoutSec = do 
  try @FobReaderAlive $ do
    doorHandler doorTimeoutSec 
  termAndClearAllResources  

doorHandler :: forall sp m. Int -> STransData m sp _ ()
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
    -- assertCheck @InvalidConditions
    {-
    on @(Not (By "failure")) $ do  
      on @InvalidConditions $
        newRes #failure (InitData ())
    -}                  
openDoorIfClosed :: Int -> STransData m sp _ ()     
openDoorIfClosed doorTimeoutSec = do
  on @("door" :? IsBinSwitchOff) $ do
    invoke #door TurnOn
    label #doorOpen
    newRes #doorTimer TimerRes
    invoke #doorTimer (StartTimer doorTimeoutSec)

closeDoor :: STransData m sp _ () 
closeDoor = do
  clear #doorTimer   
  invoke #door TurnOff
  label #doorClosed

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

mkSTransDataTypeAny "doorHandler" "DoorHandler"
mkSTransDataTypeAny "doorHandlerCon" "DoorHandlerCon"

-- :kind! Eval (DoorHandler FobReaderAlive (InitState IO () ()))
-- :kind! Eval (DoorHandlerCon NoSplitter (InitState IO () ()))
-- :kind!  ValidateSteps '[] DoorHandler FobReaderAlive (InitState IO () ())
-- :kind!  ValidateSteps '[] DoorHandlerCon FobReaderAlive (InitState IO () ())
-- :kind! StateDiagramSym  DoorHandlerCon (InitState IO () ())

{-    
evalDoorHandler = evalSTransData' (normalDoorHandler 5) (Proxy @(InitState IO))
evalDoorHandlerApp = evalSTransDataApp' (normalDoorHandler 5) (Proxy @(InitState IO))

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

vedges = vedgesSTransData' (assertDoorHandler 5) (Proxy @(InitState IO))
-}
