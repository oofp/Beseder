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

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  TwoTimersComp 
  ( TimerFunc
  , newTwoTimers
  , twoTimersRes
  , stopTwoTimers
  , invokeStopTwoTimers 
  , IsTwoTimersAlive
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import           Beseder.Base.Base                                               
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Resources.Timer
import           Beseder.Resources.Composite
import           Control.Monad.Identity (IdentityT)
import           qualified Protolude as P 

twoTimersInit :: TaskPoster m  => Int -> Int -> STransApp IdentityT m NoSplitter '[()] _ '[] ()
twoTimersInit timeoutSec1 timeoutSec2 = MkApp $ do
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec2)  

-- let constraint to be inferred and dont' look at it (it is scary)
timerHandler :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
timerHandler = do 
  on @("t1" :? IsTimerArmed :&& "t2" :? IsTimerTriggered) (invoke #t1 StopTimer)
  on @("t2" :? IsTimerArmed :&& "t1" :? IsTimerTriggered) (invoke #t2 StopTimer)

type TimerFunc =  ComposeFunc
  (CaptureFunc
      (("t1" :? IsTimerArmed) :&& ("t2" :? IsTimerTriggered))
      (InvokeAllFunc StopTimer "t1"))
  (CaptureFunc
      (("t2" :? IsTimerArmed) :&& ("t1" :? IsTimerTriggered))
      (InvokeAllFunc StopTimer "t2"))

hfunc =extractHandler timerHandler  

--noopFunc :: (Eval (TimerHnd res) ~ '[res], Monad m) => Proxy TimerHnd -> res -> m (V '[res])
--noopFunc _ res = Protolude.return (variantFromValue res)

data TimerHnd :: * -> Exp [*]
type instance Eval (TimerHnd res) = First (Eval (TimerFunc NoSplitter '[res]))

--hf :: (TaskPoster m) =>  (TimerArmed m "t1", TimerTriggered m "t2") -> m _
--hf = hfunc 

instance (TaskPoster m) => StateHandlerProv m TimerHnd (TimerArmed m "t1", TimerTriggered m "t2") where getHandler = const hfunc 
instance (TaskPoster m) => StateHandlerProv m TimerHnd (TimerTriggered m "t1", TimerArmed m "t2") where getHandler = const hfunc 
--instance (TaskPoster m) => StateHandlerProv m TimerHnd res where getHandler = noopFunc 

twoTimersRes :: TaskPoster m => Int -> Int -> m (CResPar m TimerHnd _)
twoTimersRes timeout1 timeout2 = P.return $ compRes @TimerHnd (twoTimersInit timeout1 timeout2) 

newTwoTimers :: (_) => Named tt -> Int -> Int -> STrans q m sp _ _ _ _ ()
newTwoTimers named to1 to2 = do
  res <- op $ twoTimersRes to1 to2
  newRes named res

stopTwoTimers :: (_) => m (CompReq m (TimerArmed m "t1", TimerArmed m "t2") _)
stopTwoTimers =  P.return $ compReq $ MkApp $ (invoke #t1 StopTimer >> invoke #t2 StopTimer) 

invokeStopTwoTimers :: (_) => Named name -> STrans q m sp _ _ _ _ ()
invokeStopTwoTimers named = do
  req <- op stopTwoTimers
  invoke named req


data IsTwoTimersAlive :: Type -> Exp Bool
type instance Eval (IsTwoTimersAlive t) = IsTwoTimersAliveFam (UnwrapContent t)
type family IsTwoTimersAliveFam t :: Bool where
  IsTwoTimersAliveFam (TimerArmed m "t1", TimerArmed m "t2") = 'True
  IsTwoTimersAliveFam _ = 'False


--type TTimers m = (TimerArmed m "t1", TimerArmed m "t2")
--type TTimersNext m = NextStates (TTimers m)