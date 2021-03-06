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

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  TimerApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)

timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '[()] '[] _ () 
timerHello timeoutSec1 = do              -- [()]
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  liftIO $ putStrLn ("TimerArmed"::Text)
  nextEv                                 -- [TimerTriggered "t1"]
  _t1 :: _ <- whatNext
  clear #t1                              -- [()]

timerHelloApp :: TaskPoster m  => Int -> STransApp (ContT Bool) m NoSplitter '[()] '[()] '[] () 
timerHelloApp = MkApp . timerHello

twoTimers :: TaskPoster m => Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] '[()] '[] ()
twoTimers timeoutSec1 timeoutSec2 = MkApp $ do
  liftIO $ putStrLn ("Entered twoTimersOn" :: Text)
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  newRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  handleEvents $ 
    on @("t1" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) $ do
      liftIO $ putStrLn ("Timer1 triggered"::Text)
      invoke #t2 StopTimer 
  clearAllResources
  liftIO $ putStrLn ("Leaving twoTimersOn" :: Text)

-- runAsyncTrans $ timersPump 4 7 7 9 

{-
timersPump :: TaskPoster m  => Int -> Int -> Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] '[()] '[] ()
timersPump timeoutSec1 timeoutSec2 timeoutSec3 timeoutSec4 = MkApp $ do
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  newRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  newRes #t3 TimerRes
  invoke #t3 (StartTimer timeoutSec3) 
  newRes #t4 TimerRes
  invoke #t4 (StartTimer timeoutSec4) 
  pumpEvents
  clearAllResources
-}

complexLogicTimersApp :: TaskPoster m => Int -> Int -> Int -> Int -> STransApp (ContT Bool) m NoSplitter '[()] '[()] '[] ()
complexLogicTimersApp timeoutSec1 timeoutSec2 timeoutSec3 timeoutSec4 = MkApp $ do
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  newRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2) 
  newRes #t3 TimerRes
  invoke #t3 (StartTimer timeoutSec3) 
  newRes #t4 TimerRes
  invoke #t4 (StartTimer timeoutSec4) 
  handleEvents $ do
    on @("t1" :? IsTimerTriggered :&& "t2" :? IsTimerArmed) $ 
      invoke #t2 StopTimer 
    _l1 :: _ <- whatNext
    on @("t3" :? IsTimerTriggered) $ do 
      on @("t4" :? IsTimerArmed) $ 
        invoke #t4 StopTimer 
      on @("t1" :? IsTimerArmed) $ 
        invoke #t1 StopTimer 
  _res3 :: _ <- whatNext
  clearAllResources


timerReach :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '[()] '[] _ () 
timerReach timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  reach @("t1" :? IsTimerTriggered) nextEv 
  clear #t1 

timer4 :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] '[()] '[] _ () 
timer4 timeoutSec1 = do
  newRes #t1 TimerRes 
  newRes #t2 TimerRes 
  newRes #t3 TimerRes 
  newRes #t4 TimerRes 
  invoke #t1  (StartTimer timeoutSec1)  
  invoke #t2  (StartTimer timeoutSec1)  
  invoke #t3  (StartTimer timeoutSec1)  
  invoke #t4  (StartTimer timeoutSec1)  
  nextEv' 
  nextEv' 
  nextEv' 
  nextEv' 
  clear #t1 
  clear #t2 
  clear #t3 
  clear #t4 

