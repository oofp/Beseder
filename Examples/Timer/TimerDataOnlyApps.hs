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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  TimerDataOnlyApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Beseder.Base.ControlData 
import           qualified Protolude 
import qualified Beseder.Base.Control as C                                               

timerHelloData :: Int -> STransData m NoSplitter _ _ _ _ () 
timerHelloData timeoutSec1 = do
   newRes #t1 TimerRes
   invoke #t1  (StartTimer timeoutSec1)
   nextEv 
   noop
   clear #t1

timerHelloExec :: Int -> STransData m NoSplitter '[()] _ _ _ ()   
timerHelloExec = timerHelloData

timerForeverData :: (_) => Int -> STransData m NoSplitter '[()] _ _ _ () 
timerForeverData timeoutSec1 = do
   newRes #t1 TimerRes
   invoke #t1  (StartTimer (timeoutSec1*100))
   try @("t1" :? IsTimerArmed) $ do
      forever $ do
         liftIO $ putStrLn ("Start timer2" :: Text)
         newRes #t2 TimerRes
         invoke #t2  (StartTimer timeoutSec1)
         nextEv' 
         liftIO $ putStrLn ("timer2 triggered":: Text)
         clear #t2
   invoke #t2 StopTimer
   clear #t1
   clear #t2

runTimerForeverData = runAsyncData (MkExecData (timerForeverData 2))