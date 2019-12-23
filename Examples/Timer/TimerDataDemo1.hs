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

module  TimerDataDemo1 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           qualified Protolude as P
--import           TimerDataDemo1TH

timer1 :: Int -> STransData m sp _ () 
timer1 timeoutSec1 = do                  -- [()]
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  nextEv                                 -- [TimerTriggered "t1"]
  clear #t1                              -- [()]


timer2 :: Int -> Int -> STransData m sp _ () 
timer2 timeoutSec1 timeoutSec2 = do                  -- [()]
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  nextEv                                 -- [TimerTriggered "t1"]
  clear #t1                              -- [()]

timer2a :: Int -> Int -> STransData m sp _ () 
timer2a timeoutSec1 timeoutSec2 = do                  -- [()]
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  newRes #t2 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t2  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  label #init
  nextEv                                 -- [TimerTriggered "t1"]
  label #tick1
  nextEv
  label #tick2
  clear #t1                              -- [()]
  clear #t2  
    
--reifyValue "timer1"

type TimerDataFunc m =
  (ComposeFunc
  (NewResFunc TimerRes "t1" m)
  (ComposeFunc
     (InvokeAllFunc StartTimer "t1")
     (ComposeFunc
        (CaptureFunc Dynamics GetNextAllFunc)
        (ClearAllFunc "t1"))))

-- :kind! Eval ((TimerDataFunc IO) NoSplitter '[()])        
-- :t evalSTransData (timer1 1)
-- :t edgesSTransData (timer1 1)
-- :t statesAndLabels (timer1 1)
-- vedgesSTransData (timer2a 1 1)
--runTimer1 :: IO ()  
--runTimer1 = runAsyncData $ (timer1 2)

-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


func ::  Q ()
func  = P.return ()
