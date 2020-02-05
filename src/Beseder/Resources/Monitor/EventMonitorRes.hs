{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.Resources.Monitor.EventMonitorRes where

import           Protolude  
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data StopEvMonitor = StopEvMonitor deriving Show
data AckEvent = AckEvent deriving Show

class Monad m => EventMonitorProv m res where
  data  WaitForEvent m res 
  data  EventReceived m res 
  data  EvMonitorStopped m res 
  data  ResPar m res 

  createEvMonitor :: MkResDef m (ResPar m res) (WaitForEvent m res)
  stopEvMonitorWait :: RequestDef m StopEvMonitor (WaitForEvent m res) '[EvMonitorStopped m res]  
  stopEvMonitorReceived :: RequestDef m StopEvMonitor (EventReceived m res) '[EvMonitorStopped m res]  
  ackEvent :: RequestDef m AckEvent (EventReceived m res) '[WaitForEvent m res]  
  evTransition :: TransitionDef m (WaitForEvent m res) '[EventReceived m res]
  termEvMonitor :: TermDef m (EvMonitorStopped m res)  

buildRes ''EventMonitorProv

type instance TermRequest (St (WaitForEvent m res) name) = StopEvMonitor
type instance TermRequest (St (EventReceived m res) name) = StopEvMonitor

instance GetInstance StopEvMonitor where getInstance = StopEvMonitor