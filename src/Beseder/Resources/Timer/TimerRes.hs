{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}

module Beseder.Resources.Timer.TimerRes 
  ( TimerRes (..)
  , StartTimer (..)
  , StopTimer (..)
  , TimerProv (..)
  , TimerNotArmed 
  , TimerArmed 
  , TimerTriggered 
  , TimerStopped 
  , IsTimerArmed 
  , IsTimerNotArmed 
  , IsTimerTriggered 
  ) where

import           Protolude    
import           Control.Monad.Cont
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common

data TimerRes = TimerRes deriving Show
newtype StartTimer = StartTimer Int deriving (Show, Eq)
data StopTimer = StopTimer deriving (Show, Eq)
instance GetInstance StopTimer where
  getInstance = StopTimer
instance GetInstance TimerRes where
  getInstance = TimerRes
  
class TimerProv m where
  data TimerNotArmedEvData m  
  data TimerArmedEvData m  
  data TimerTriggeredEvData m
  data TimerStoppedEvData m
  
  createTimer :: TimerRes -> m (TimerNotArmedEvData m)

  startTimer :: StartTimer -> (TimerNotArmedEvData m) -> m (TimerArmedEvData m)
  stopTimer :: StopTimer -> (TimerArmedEvData m) -> m (TimerStoppedEvData m)

  clearNotArmedTimer :: TimerNotArmedEvData m -> m ()
  clearStoppedTimer :: TimerStoppedEvData m -> m ()
  clearTriggeredTimer :: TimerTriggeredEvData m -> m ()
  
  timerTransition :: (TimerArmedEvData m) -> (TimerTriggeredEvData m -> m Bool) -> m Bool

--
type TimerNotArmed m name = St (TimerNotArmedEvData m) name  
type TimerArmed m name = St (TimerArmedEvData m) name
type TimerTriggered m name = St (TimerTriggeredEvData m) name
type TimerStopped m name = St (TimerStoppedEvData m) name

instance (MonadIO m, TimerProv m) => CreateRes m name TimerRes (V '[TimerNotArmed m name])  where
  createRes _nm timer = fmap (variantFromValue . St) (createTimer timer)  

instance (MonadIO m, TimerProv m) => MkRes m TimerRes where
  type ResSt m TimerRes = TimerNotArmedEvData m
  mkRes timer = createTimer timer  

type instance StateTrans (TimerNotArmedEvData m) = 'Static
type instance StateTrans (TimerTriggeredEvData m) = 'Static
type instance StateTrans (TimerStoppedEvData m) = 'Static
type instance StateTrans (TimerArmedEvData m) = 'Dynamic

type instance StateTrans (TimerNotArmed m name) = 'Static
type instance StateTrans (TimerStopped m name) = 'Static
type instance StateTrans (TimerTriggered m name) = 'Static
type instance StateTrans (TimerArmed m name) = 'Dynamic

type instance TermRequest (TimerArmed m name) = StopTimer

type family IsTimerArmedFam a :: Bool where
  IsTimerArmedFam (TimerArmed _ _) = 'True
  IsTimerArmedFam _ = 'False
  
data IsTimerArmed :: Type -> Exp Bool 
type instance Eval (IsTimerArmed a) = IsTimerArmedFam a

type family IsTimerNotArmedFam a :: Bool where
  IsTimerNotArmedFam (TimerNotArmed _ _) = 'True
  IsTimerNotArmedFam _ = 'False

data IsTimerNotArmed :: Type -> Exp Bool 
type instance Eval (IsTimerNotArmed a) = IsTimerNotArmedFam a
  
type family IsTimerTriggeredFam a :: Bool where
  IsTimerTriggeredFam (TimerTriggered _ _) = 'True
  IsTimerTriggeredFam _ = 'False
  
data IsTimerTriggered :: Type -> Exp Bool 
type instance Eval (IsTimerTriggered a) = IsTimerTriggeredFam a

instance (MonadIO m, TimerProv m) => Request m StartTimer (TimerNotArmed m name) where
  type instance ReqResult StartTimer (TimerNotArmed m name) = '[TimerArmed m name]
  request startTimerPars (St evData) = fmap (variantFromValue . St) (startTimer startTimerPars evData) 

instance (MonadIO m, TimerProv m) => Request m StopTimer (TimerArmed m name) where
  type instance ReqResult StopTimer (TimerArmed m name) = '[TimerStopped m name]
  request stopTimerPars (St evData) = fmap (variantFromValue . St) (stopTimer stopTimerPars evData) 
  
instance (MonadIO m, TimerProv m) => TermState m (TimerNotArmed m name) where
  terminate (St evData) = clearNotArmedTimer evData
instance (MonadIO m, TimerProv m) => TermState m (TimerStopped m name) where
  terminate (St evData) = clearStoppedTimer evData
instance (MonadIO m, TimerProv m) => TermState m (TimerTriggered m name) where
  terminate (St evData) = clearTriggeredTimer evData
                    
instance (MonadIO m, TimerProv m) => Transition m (TimerArmed m name) where
  type instance NextStates (TimerArmed m name) = '[TimerTriggered m name]
  next (St evData) cbFunc = timerTransition evData (\nextEvData -> cbFunc (variantFromValue (St nextEvData)))
  
  
