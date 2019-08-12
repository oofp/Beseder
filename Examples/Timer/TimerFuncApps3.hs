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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerFuncApps3 where


import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)
import           System.Random


data Dict1 

instance TransDict q m Dict1 "getStartTimer" StartTimer where
  getTransFromDict _ _ =  MkApp $ Beseder.Base.Control.return (StartTimer 5)

--instance (MonadIO m, MonadTrans q, Monad (q m)) => TransDict q m Dict1 "getRandonTimer" StartTimer where
instance (MonadIO m) => TransDict (ContT Bool) m Dict1 "getRandomTimer" StartTimer where  
  getTransFromDict _ _ =  MkApp $ do 
    timeoutSec <- liftIO $ randomRIO (1,10)
    Beseder.Base.Control.return (StartTimer timeoutSec)

type InitAndStartTimer name m = 
  NewResFunc TimerRes name m
  :>> DictFunc "getStartTimer" :>>= InvokeAllFunc StartTimer name

type TimerBasicFunc m = 
  InitAndStartTimer "t1" m 
  :>> InitAndStartTimer "t2" m 
  :>> InitAndStartTimer "t3" m 
  :>> InitAndStartTimer "t4" m 
  :>> Trace "before pump"
  :>> PumpEvents
  :>> Trace "after pump"
  :>> ClearResourcesExcept '["log"]

type InitAndStartRandomTimer name m = 
  NewResFunc TimerRes name m
  :>> DictFunc "getRandomTimer" :>>= Invoke name StartTimer
  
type TimerHandlingFunc m = 
  InitAndStartRandomTimer "t1" m 
  :>> InitAndStartRandomTimer "t2" m 
  :>> InitAndStartRandomTimer "t3" m 
  :>> InitAndStartRandomTimer "t4" m 
  :>> Trace "before handling"
  :>> HandleEvents 
    ( Trace "inside loop"
    -- :>> Next
    :>> On ("t1" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) 
      (Invoke "t2" StopTimer)
    :>> On ("t2" :? IsTimerTriggered :&& "t3" :? IsTimerArmed) 
      (Invoke "t3" StopTimer)
    :>> On ("t3" :? IsTimerTriggered :&& "t4" :? IsTimerArmed) 
      (Invoke "t4" StopTimer)
    :>> On ("t4" :? IsTimerTriggered :&& "t1" :? IsTimerArmed) 
      (Invoke "t1" StopTimer)
    -- :>> Trace "inside loop (2)"  
    ) 
  :>> Trace "after pump"
  :>> ClearAllResourcesButTrace

type TimerTestFunc m = 
  InitAndStartRandomTimer "t1" m 
  :>> InitAndStartRandomTimer "t2" m 
  :>> Trace "before handling"
  :>> HandleEvents 
    ( Trace "inside loop"
    ) 
  :>> Trace "after pump"
  :>> ClearAllResourcesButTrace
  
{-  
-- :kind! EvalTransFunc IO TimerBasicFunc
-- :kind! EvalTransFuncWithTrace IO TimerBasicFunc
executableTrans :: 
  ( TaskPoster m
  ) => ExcecutableTrans (ContT Bool) m (TimerHandlingFunc m) 
executableTrans = buildTrans @Dict1

runTimerBasic :: IO ()
runTimerBasic = runAsyncTrans executableTrans
-}

--reifyTimerBasicApp :: STransApp (ContT Bool) TaskQ NoSplitter '[()] '(('[()]),'[]) () 
--reifyTimerBasicApp = MkApp $ reifyAsyncTrans (Proxy @(TimerBasicFunc TaskQ)) (Proxy @Dict1) 



