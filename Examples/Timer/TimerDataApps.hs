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

module  TimerDataApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
--import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Beseder.Base.ControlData 
--import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
--                                               (>>), (>>=), forever, until,try,on)

timerHelloData :: Int -> STransData m NoSplitter _ () 
timerHelloData timeoutSec1 = do
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  nextEv' 
  clear #t1


timerHello :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello timeoutSec1 = interpret (timerHelloData timeoutSec1)

timerHelloData2 :: Int -> STransData m NoSplitter _ () 
timerHelloData2 timeoutSec1 = do
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes
  invoke #t2  (StartTimer timeoutSec1)
  nextEv' 
  nextEv' 
  clear #t1
  clear #t2


timerHello2 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello2 timeoutSec1 = interpret (timerHelloData2 timeoutSec1)

timerHelloData4 :: Int -> STransData m NoSplitter _ () 
timerHelloData4 timeoutSec1 = do
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes
  invoke #t4  (StartTimer timeoutSec1)
  nextSteps @4
  clear #t1
  clear #t2
  clear #t3
  clear #t4

timerHello4 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello4 timeoutSec1 = interpret (timerHelloData4 timeoutSec1)
  
timerHelloDataTry4 :: Int -> STransData m NoSplitter _ () 
timerHelloDataTry4 timeoutSec1 = do
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes
  invoke #t4  (StartTimer timeoutSec1)
  try @("t1" :? IsTimerArmed) $ do
    nextEv' 
    nextEv' 
    nextEv' 
  on @("t2" :? IsTimerArmed) $ do
    invoke #t2 StopTimer  
  nextEv 
  skipAll
  clear #t1
  clear #t2
  clear #t3
  clear #t4

timerHelloTry4 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHelloTry4 timeoutSec1 = interpret (timerHelloDataTry4 timeoutSec1)
  
timerHelloDataSkipTo4 :: Int -> STransData m NoSplitter _ () 
timerHelloDataSkipTo4 timeoutSec1 = do
  liftIO $ putStrLn ("started"::Text)
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  newRes #t2 TimerRes
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes
  invoke #t3  (StartTimer timeoutSec1)
  newRes #t4 TimerRes
  invoke #t4  (StartTimer timeoutSec1)
  try @("t2" :? IsTimerArmed) $ do
    skipTo @("t1" :? IsTimerTriggered) 
    liftIO $ putStrLn ("try/skipTo completed"::Text)
  on @("t2" :? IsTimerArmed :&& "t1" :? IsTimerTriggered) $
    invoke #t2 StopTimer  
  on @("t1" :? IsTimerArmed) $
    invoke #t1 StopTimer  
  skipAll
  clearAllResources
  liftIO $ putStrLn ("done"::Text)

--timerHelloSkip4 :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
--timerHelloSkip4 timeoutSec1 = interpret (timerHelloDataSkipTo4 timeoutSec1)

runTimerHelloSkip4 :: IO ()
runTimerHelloSkip4 = runAsyncData (timerHelloDataSkipTo4 2)