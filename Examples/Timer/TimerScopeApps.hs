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

module  TimerScopeApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Resources.State.DataRes 
import           Data.String 
import           Control.Monad.Cont (ContT)
import           qualified Protolude 

timerClearRes :: Int -> STransData m NoSplitter  _ () 
timerClearRes timeoutSec1 = do              
  newRes #t1 TimerRes                    
  invoke #t1  (StartTimer timeoutSec1)   
  newRes #t2 TimerRes
  clearResources (Proxy @["t1","t2"])


timerScope :: Int -> STransData m NoSplitter  _ () 
timerScope timeoutSec1 = do              
  newRes #t1 TimerRes                    
  newRes #t2 TimerRes                    
  invoke #t1  (StartTimer timeoutSec1)   
  invoke #t2  (StartTimer timeoutSec1)
  nextEv   
  scopeRes $ do
    newRes #t3 TimerRes
    newRes #t4 TimerRes
    invoke #t3  (StartTimer timeoutSec1)   
    invoke #t4  (StartTimer timeoutSec1)
    nextEv
    on @("t4" :? IsTimerTriggered) $ clear #t4   
    on @("t1" :? IsTimerTriggered) $ clear #t1   

interpretTimerScope :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()    
interpretTimerScope = interpret (timerScope 1)    


timerScopePump :: Int -> STransData m NoSplitter  _ () 
timerScopePump timeoutSec1 = do              
  newRes #t1 TimerRes                    
  newRes #t2 TimerRes                    
  invoke #t1  (StartTimer timeoutSec1)   
  invoke #t2  (StartTimer timeoutSec1)
  scopeRes $ do
    newRes #t3 TimerRes                    
    newRes #t4 TimerRes                    
    invoke #t3  (StartTimer timeoutSec1)   
    invoke #t4  (StartTimer timeoutSec1)
    pumpEvents
  clearAllBut (Proxy @'["t2"])  

interpretTimerScopePump :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()    
interpretTimerScopePump = interpret (timerScopePump 1)  


timerClearBut :: Int -> STransData m NoSplitter  _ () 
timerClearBut timeoutSec1 = do              
  newRes #t1 TimerRes                    
  newRes #t2 TimerRes                    
  invoke #t1  (StartTimer timeoutSec1)   
  invoke #t2  (StartTimer timeoutSec1)
  newRes #t3 TimerRes                    
  newRes #t4 TimerRes                    
  invoke #t3  (StartTimer timeoutSec1)   
  invoke #t4  (StartTimer timeoutSec1)
  pumpEvents
  clearAllBut (Proxy @'["t2","t3"])  


interpretTimerClearBut :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()    
interpretTimerClearBut = interpret (timerClearBut 1)    