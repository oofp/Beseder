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

module  TimerDataOnOrElse where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

twoTimers :: TaskPoster m => Int -> Int -> STransData m NoSplitter _ ()
twoTimers timeoutSec1 timeoutSec2 = do
  liftIO $ putStrLn ("Entered twoTimersOn" :: Text)
  newRes #t1 TimerRes 
  invoke #t1 (StartTimer timeoutSec1)  
  newRes #t2 TimerRes
  invoke #t2 (StartTimer timeoutSec2)
  nextEv
  onOrElse @("t1" :? IsTimerTriggered) 
    (liftIO $ putStrLn ("1st timer triggered"::Text)) 
    (liftIO $ putStrLn ("2nd timer triggered"::Text))
  nextEv   
  clearAllResources
  
runTimers :: IO ()  
runTimers = runAsyncData $ (twoTimers 4 2)

runTimers2 :: IO ()  
runTimers2 = runAsyncData $ (twoTimers 2 4)


-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


