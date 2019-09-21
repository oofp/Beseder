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

module  TimerApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           qualified Protolude 
import           Beseder.Base.Internal.STransFunc

type TimerHelloFunc m =
  (ComposeFunc
  (WithResAllFunc (TimerNotArmed m "t1"))
  (ComposeFunc
     (InvokeAllFunc StartTimer "t1")
     (ComposeFunc
        (CaptureFunc
           (By 'Dynamic) GetNextAllFunc)
        (ClearAllFunc "t1"))))

type CompletedRes = '(('[()]),'[])

type TimerHelloFuncNicer m = 
  NewResFunc TimerRes "t1" m 
  :>> InvokeAllFunc StartTimer "t1" 
  :>> CaptureFunc (By 'Dynamic) GetNextAllFunc 
  :>> ClearAllFunc "t1"  
-- :kind! EvalTransFunc IO TimerHelloFunc

data AnotherReq = AnotherReq deriving (Show,Eq)

--timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[TimerNotArmed m "t1"] '(('[TimerArmed m "t1"]),'[]) (InvokeAllFunc StartTimer "t1") () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
--timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[TimerNotArmed m "t1"] _ (InvokeAllFunc StartTimer "t1") () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHello timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  nextEv 
  clear #t1 

timerHelloApp :: TaskPoster m  => Int -> STransApp (ContT Bool) m NoSplitter '[()] '(('[()]),'[]) () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
timerHelloApp = MkApp . timerHello

-- runAsyncTrans $ twoTimersOn 4 7  
-- runAsyncTrans (instrumentTrans varIndexInstr  (twoTimersOn 5 3))
-- runAsyncTrans (instrumentTrans varLengthInstr  (twoTimersOn 5 3))
twoTimersOn :: TaskPoster m => Int -> Int -> AsyncTransApp m _ _
twoTimersOn timeoutSec1 timeoutSec2 = do
  liftIO $ putStrLn ("Entered twoTimersOn" :: Text)
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  withRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  handleEvents $ do
    on @("t1" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) $ do
      liftIO $ putStrLn ("Timer1 triggered"::Text)
      invoke #t2 StopTimer 
  clearAllResources
  liftIO $ putStrLn ("Leaving twoTimersOn" :: Text)

-- runAsyncTrans $ timersPump 4 7 7 9 
{- 
timersPump :: TaskPoster m  => Int -> Int -> Int -> Int -> AsyncTransApp m _ _ 
timersPump timeoutSec1 timeoutSec2 timeoutSec3 timeoutSec4 = do
  withRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  withRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  withRes #t3 TimerRes
  invoke #t3 (StartTimer timeoutSec3) 
  withRes #t4 TimerRes
  invoke #t4 (StartTimer timeoutSec4) 
  pumpEvents
  clearAllResources
-}

{-
complexLogicTimersApp :: TaskPoster m => Int -> Int -> Int -> Int -> AsyncTransApp m _ _
complexLogicTimersApp timeoutSec1 timeoutSec2 timeoutSec3 timeoutSec4 = do
  withRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  withRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  withRes #t3 TimerRes
  invoke #t3 (StartTimer timeoutSec3) 
  withRes #t4 TimerRes
  invoke #t4 (StartTimer timeoutSec4) 
  handleEvents $ do
    on @("t1" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) $ do
      invoke #t2 StopTimer 
    _l1 :: _ <- whatNext
    on @("t3" :? IsTimerTriggered) $ do 
      on @("t4" :? IsTimerArmed) $ do
        invoke #t4 StopTimer 
      on @("t1" :? IsTimerArmed) $ do
        invoke #t1 StopTimer 
  res3 :: _ <- whatNext
  clearAllResources
-}


twoTimersWait :: TaskPoster m => Int -> Int -> AsyncTransApp m _ _
twoTimersWait timeoutSec1 timeoutSec2 = do
  liftIO $ putStrLn ("Entered twoTimersOn" :: Text)
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  withRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  on @Dynamics
    (on @("t1" :? IsTimerTriggered) waitFor)