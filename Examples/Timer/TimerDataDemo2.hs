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

module  TimerDataDemo2 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

timer1 :: Int -> STransData m sp _ () 
timer1 timeoutSec1 = do                  
  newRes #t1 TimerRes                    
  invoke #t1  (StartTimer timeoutSec1)   
  newRes #t2 TimerRes                    
  invoke #t2  (StartTimer timeoutSec1)   
  handleEvents $ do
    on @("t1" :? IsTimerTriggered) $ do
      invoke #t2 StopTimer
  clear #t1
  clear #t2                              



timerForever :: STransData TaskQ NoSplitter _ _
timerForever = do
  newRes #t TimerRes
  invoke #t (StartTimer 36)
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 3)
  try @(Dynamics) $ do
    try @("t" :? IsTimerArmed) $ do
      forever $ do
        nextEv'
        liftIO $ putStrLn ("handleEvents ...." :: Text)
        on @("t1" :? IsTimerTriggered) $ do 
          clear #t1
          newRes #t1 TimerRes
          invoke #t1 (StartTimer 3)
  liftIO $ putStrLn ("loop is completed" :: Text)
  invoke #t1 StopTimer
  clearAllResources    
  
-- :t evalSTransData timer1 1)
-- :t validateSTransData timerForever

--runTimer1 :: IO ()  
--runTimer1 = runAsyncData $ (timer1 2)

-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


