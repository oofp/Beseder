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
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

timer1 :: TaskPoster m  => Int -> AsyncApp m '[()] '[()] () 
timer1 timeoutSec1 = MkApp $ do          -- [()]
  liftIO $ putStrLn ("Entered timerHello"::Text)
  newRes #t1 TimerRes                    -- [TimerNotArmed "t1"]
  _t0 :: _ <- whatNext
  invoke #t1  (StartTimer timeoutSec1)   -- [TimerArmed "t1"]
  liftIO $ putStrLn ("TimerArmed"::Text)
  nextEv                                 -- [TimerTriggered "t1"]
  _t1 :: _ <- whatNext
  liftIO $ putStrLn ("TimerTriggered"::Text)
  clear #t1                              -- [()]

runTimer1 :: IO ()  
runTimer1 = runAsyncApp $ (timer1 2)

-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


