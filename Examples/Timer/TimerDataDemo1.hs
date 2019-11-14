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

module  TimerDemo1 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

timer1 :: Int -> STransData m sp _ () 
timer1 timeoutSec1 = do                  -- [()]
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  nextEv                                 -- [TimerTriggered "t1"]
  clear #t1                              -- [()]


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


--runTimer1 :: IO ()  
--runTimer1 = runAsyncData $ (timer1 2)

-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


