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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  ThreeTimersApps  where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import           Beseder.Base.Base                                               
import           Beseder.Misc.Misc                                               
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
--import           Beseder.Resources.State.DataRes
import           Beseder.Resources.Timer
import           ThreeTimersComp
import           Beseder.Resources.Composite
import           Control.Monad.Cont (ContT)

threeTimerRes :: ThreeTimersRes
threeTimerRes = CRdPar (ThreeTimers 1 2 3)

threeTimerApps :: STransData m NoSplitter _ ()
threeTimerApps = do
  newRes #tt threeTimerRes
  pumpEvents
  clear #tt

interpretThreeTimerApps :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()
interpretThreeTimerApps = interpret threeTimerApps 

threeTimersAndTimerApp :: STransData m NoSplitter _ ()
threeTimersAndTimerApp = do
  newRes #tt threeTimerRes
  newRes #t TimerRes
  invoke #t (StartTimer 5)
  handleEvents $
    on @("t" :? IsTimerTriggered) $ do
      invoke #tt StopAllTimers
  clearAllResources    

interpretThreeTimersAndTimerApp :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()
interpretThreeTimersAndTimerApp = interpret threeTimersAndTimerApp 

threeTimersTwiceApp :: STransData m NoSplitter _ ()
threeTimersTwiceApp = do
  newRes #tt1 threeTimerRes
  newRes #tt2 threeTimerRes
  pumpEvents
  --clearAllResources    

threeTimersTwiceSkipApp :: STransData m NoSplitter _ ()
threeTimersTwiceSkipApp = do
  newRes #tt1 threeTimerRes
  newRes #tt2 threeTimerRes
  skipAll
  --clearAllResources    
  
interpretThreeTimersTwiceApp :: STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()
interpretThreeTimersTwiceApp = interpret threeTimersTwiceSkipApp 
  