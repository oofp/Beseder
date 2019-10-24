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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  TimerOrderApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Common                                               
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common (TaskPoster)
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Beseder.Base.Internal.STransIxDo
import           Haskus.Utils.Variant

type T2T1 = '["t2","t1"]
{-

timerOrder :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerOrder timeoutSec1 = do
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1)  
  nextEv'
  on @("t2" :? IsTimerArmed) (order (Proxy @T2T1))

--runHello :: IO ()
--runHello = runAsyncTrans (timerHello 5)

--stopAndClear :: forall m n1 n2. (TaskPoster m, KnownSymbol n1, KnownSymbol n2) => STrans (ContT Bool) m NoSplitter '[(TimerArmed m (n1 :: Symbol),TimerTriggered m (n2 :: Symbol))] _ _ _ ()
--stopAndClear :: (TaskPoster m) => STrans (ContT Bool) m NoSplitter '[(TimerArmed m "n1",TimerTriggered m "n2")] _ _ _ ()
stopAndClear :: (TaskPoster m, KnownSymbol n1, KnownSymbol n2) => Named n1 -> Named n2 -> STrans (ContT Bool) m NoSplitter '[(TimerArmed m n1,TimerTriggered m n2)] _ '[] _ ()
stopAndClear nm1 nm2 = do
  --let nm2 :: Named n2
  --    nm2 = Named
  --invoke nm1 StopTimer
  --clear nm2 
  noop

type family TypeFound (a :: *) (b :: * ) :: Bool where
  TypeFound (Proxy a) a = 'True
  TypeFound (Proxy a) b = 'False
  TypeFound (Proxy a, next) b = TypeFound next b
-}

    

timerOrder :: forall n1 n2 m. (KnownSymbol n1, KnownSymbol n2, TaskPoster m, _)  => Int -> Named n1 -> Named n2 -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerOrder timeoutSec1 t1 t2 = do
  newRes t1 TimerRes 
  invoke t1  (StartTimer timeoutSec1)  
  newRes t2 TimerRes 
  invoke t2  (StartTimer timeoutSec1)  
  nextEv
  --skipAll
  on @(n1 :? IsTimerArmed) (invoke t1 StopTimer)
  nextEv
  clear t1
  clear t2

timeOrderCall = timerOrder 10 #n1 #n2  


timerOrder2 :: forall n1 n2 n3 m. (KnownSymbol n1, KnownSymbol n2, KnownSymbol n3, TaskPoster m, _)  => Named n1 -> Named n2 -> Named n3 -> STrans (ContT Bool) m NoSplitter _ _ _ _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerOrder2 t1 t2 t3 = do
  nextEv
  --skipAll
  on @(n1 :? IsTimerArmed) $ do 
    invoke t1 StopTimer
    newRes t3 TimerRes
    invoke t3 (StartTimer 2)
  nextEv
  --clear t1
  --clear t2

timeOrderCall2a :: TaskPoster m => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timeOrderCall2a timeoutSec1 = do
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1) 
  newRes #t3 TimerRes 
  invoke #t3  (StartTimer timeoutSec1) 
  timerOrder2 #t1 #t2 #t4  
  nextEv
  -- clear #t3

timeOrderCall2b :: TaskPoster m => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timeOrderCall2b timeoutSec1 = do
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  newRes #t2 TimerRes 
  invoke #t2  (StartTimer timeoutSec1) 
  newRes #t5 TimerRes 
  invoke #t5  (StartTimer timeoutSec1) 
  newRes #t6 TimerRes 
  invoke #t6  (StartTimer timeoutSec1) 
  timerOrder2 #t2 #t1 #t3  
  skipAll
  clearAllResources
