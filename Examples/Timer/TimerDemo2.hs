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

module  TimerDemo2 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

twoTimers :: TaskPoster m => Int -> Int -> AsyncApp m '[()] '[()] ()
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

runTimersTriggered :: IO ()  
runTimersTriggered = runAsyncApp $ (twoTimers 2 4)
  
runTimers :: IO ()  
runTimers = runAsyncApp $ (twoTimers 4 2)

-- $> runTimer1
-- ghcid --command "stack ghci beseder-examples:exe:TimerApps"


