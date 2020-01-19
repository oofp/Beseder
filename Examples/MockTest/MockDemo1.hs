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
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  MockDemo1 where

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
import           Beseder.Resources.Monitor.BinaryMonitorRes
import           Beseder.Resources.Monitor.Impl.BinaryMonitorMock
import           GHC.Exts (Any)    

mock1 :: (_) => STransData m sp _ () 
mock1 = do           
  (op $ mkBinaryMonitorMock (3,5)) >>= newRes #mon  
  nextEv                      
  clear #mon                   
  --newRes #t TimerRes
  --invoke #t (StartTimer 1)
  --nextEv                                

mock1a :: (_) => STransData m sp _ () 
mock1a = do           
  (op $ mkBinaryMonitorMock (10,12)) >>= newRes #mon  
  newRes #t TimerRes
  invoke #t (StartTimer 1)     
  nextEv                      
  clear #t                         
  invoke #mon StopMonitor          
  clear #mon                   
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 1)     
  nextEv                      
  clear #t1      

mock1b :: (_) => STransData m sp _ () 
mock1b = do           
  (op $ mkBinaryMonitorMock (3,6)) >>= newRes #mon  
  nextEv                      
  invoke #mon StopMonitor          
  clear #mon                   
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 1)     
  nextEv                      
  clear #t1      

mock1c :: (_) => Int -> STransData m sp _ () 
mock1c testDuration = do           
  (op $ mkBinaryMonitorMock (3,6)) >>= newRes #mon  
  (op $ mkBinaryMonitorMock (2,4)) >>= newRes #mon1  
  (op $ mkBinaryMonitorMock (1,9)) >>= newRes #mon2  
  newRes #t TimerRes
  invoke #t (StartTimer testDuration)
  try @("t" :? IsTimerArmed) pumpEvents  
  invoke #mon StopMonitor          
  clear #mon                   
  clear #mon1                   
  clear #mon2                   
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 1)     
  nextEv                      
  clear #t1      
  clear #t      


runMock1 :: IO ()  
runMock1 = runAsyncData mock1

runMock1a :: IO ()  
runMock1a = runAsyncData mock1a

runMock1b :: IO ()  
runMock1b = runAsyncData mock1b


mkSTransDataTypeAny "mock1c" "Mock1C"

-- :kind!  ValidateSteps '[] Mock1C NoSplitter '[()]
runMock1c :: Int -> IO ()  
runMock1c = runAsyncData . mock1c
