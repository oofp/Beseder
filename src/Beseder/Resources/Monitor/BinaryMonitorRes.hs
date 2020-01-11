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

module Beseder.Resources.Monitor.BinaryMonitorRes where

import           Protolude  
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

data StopMonitor = StopMonitor deriving Show

class Monad m => BinaryMonitorProv m res where
  data  BinMonitorOn m res 
  data  BinMonitorOff m res 
  data  BinMonitorStopped m res 
  data  ResPar m res 

  createBinaryMonitor :: MkResDef m (ResPar m res) (BinMonitorOff m res)
  stopBinaryMonitorOn :: RequestDef m StopMonitor (BinMonitorOn m res) '[BinMonitorStopped m res]  
  stopBinaryMonitorOff :: RequestDef m StopMonitor (BinMonitorOff m res) '[BinMonitorStopped m res]  
  onOffTransition :: TransitionDef m (BinMonitorOn m res) '[BinMonitorOff m res]
  offOnTransition :: TransitionDef m (BinMonitorOff m res) '[BinMonitorOn m res]
  termBinaryMonitor :: TermDef m (BinMonitorStopped m res)  

buildRes ''BinaryMonitorProv

type instance TermRequest (St (BinMonitorOn m res) name) = StopMonitor
type instance TermRequest (St (BinMonitorOff m res) name) = StopMonitor
