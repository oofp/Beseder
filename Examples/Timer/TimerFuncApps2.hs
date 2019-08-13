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
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerFuncApps2 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Resources.State.StateLogger 
import           Beseder.Resources.State.DataRes
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)

data Dict1 = Dict1

instance TransDict q m Dict1 "getStartTimer" xs StartTimer where
  getTransFromDict _ _ =  MkApp $ Beseder.Base.Control.return (StartTimer 5)

instance TransDict q m Dict1 "initData" xs (InitData Int) where
  getTransFromDict _ _ =  MkApp $ Beseder.Base.Control.return (InitData 0)
  
instance 
  TransDict q m Dict1 "incCounter" xs (ModifyData Int Int) where
    getTransFromDict _ _ =  MkApp $ return (ModifyData (+1))

instance (Monad (q m), MonadTrans q, GetTypeByNameVar "counter" (StD Int "counter") xs) =>
  TransDict q m Dict1 "setCounter" xs (SetData Int) where
    getTransFromDict _ _ =  MkApp $ do
      cnt <- gets #counter getData 
      return (SetData (cnt+1))
      
type TimerBasicFunc m = 
  NewResFunc TimerRes "t1" m 
  :>> DictFunc "getStartTimer" :>>= InvokeAllFunc StartTimer "t1"
  :>> Trace "log0"
  :>> GetNextAllFunc 
  :>> Trace "log1"
  :>> ClearAllResourcesButTrace

type InitAndStartTimer name m = 
  NewResFunc TimerRes name m
  :>> PutStrLn name
  :>> DictFunc "getStartTimer" :>>= InvokeAllFunc StartTimer name

type TimerBasicFunc2 m = 
  PutStrLn "Entered TimerBasicFunc2"
  :>> DictFunc "initData" :>>= NewResFunc (InitData Int) "counter"m
  :>> InitAndStartTimer "t1" m
  :>> InitAndStartTimer "t2" m
  :>> Trace "log0"
  :>> Next 
  :>> PutStrLn "After 1st Next"
  :>> On ("t1" :? IsTimerArmed)  
    (   Trace "log1a"
    :>> "t1" :-> StopTimer
    :>> DictFunc "incCounter" :>>= Invoke "counter" (ModifyData Int Int)
    :>> DictFunc "setCounter" :>>= Invoke "counter" (SetData Int)
    :>> Trace "log1b"
    ) --  --
  :>> Trace "log1"
  :>> Next
  :>> Trace "log2"
  :>> ClearAllResourcesButTrace
  :>> PutStrLn "Leaving TimerBasicFunc2"
  

-- :kind! EvalTransFunc IO TimerBasicFunc
-- :kind! EvalTransFuncWithTrace IO TimerBasicFunc

executableTrans :: (TaskPoster m) => ExcecutableTrans (ContT Bool) m (TimerBasicFunc m) 
executableTrans = buildTrans Dict1

runTimerBasic :: IO ()
runTimerBasic = runAsyncTrans executableTrans

executableTrans2 :: (TaskPoster m) => ExcecutableTrans (ContT Bool) m (TimerBasicFunc2 m) 
executableTrans2 = buildTrans Dict1

runTimerBasic2 :: IO ()
runTimerBasic2 = runAsyncTrans executableTrans2

--reifyTimerBasic :: (_) => STrans (ContT Bool) TaskQ NoSplitter xs (Eval ((TimerBasicFunc TaskQ) NoSplitter xs)) (TimerBasicFunc TaskQ) () 
--reifyTimerBasic = reifyAsyncTrans (Proxy @(TimerBasicFunc TaskQ)) (Proxy @Dict1) 

reifyTimerBasicApp :: STransApp (ContT Bool) TaskQ NoSplitter '[()] '(('[()]),'[]) () 
reifyTimerBasicApp = MkApp $ reifyAsyncTrans (Proxy @(TimerBasicFunc TaskQ)) Dict1 
