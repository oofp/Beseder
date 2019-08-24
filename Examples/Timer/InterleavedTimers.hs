{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  InterleavedTimers where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Resources.State.DataRes
import           Beseder.Resources.Composite
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Control.Monad.Identity (IdentityT)
import           qualified Protolude 


data ITState (timer1 :: Bool) (timer2 :: Bool) = ITState deriving Show

type family ToggleTimer (firstOrSecond :: Bool) (st :: *) where
  ToggleTimer True (ITState False t2) = ITState True t2
  ToggleTimer True (ITState True t2) = ITState False t2
  ToggleTimer False (ITState t1 False) = ITState t1 True
  ToggleTimer False (ITState t1 True) = ITState t1 False
   

itRes :: Int -> Int -> STrans IdentityT TaskQ NoSplitter '[()] _ _ ()
itRes to1 to2 = do 
  newRes #rs (InitData (ITState @True @True)) 
  newRes #t1 TimerRes >> newRes #t2 TimerRes
  invoke #t1 (StartTimer to1)
  invoke #t2 (StartTimer to2)

{-  
startTwoTimersReq :: Int -> Int -> STrans IdentityT TaskQ NoSplitter '[(TimerNotArmed TaskQ "t1",TimerNotArmed TaskQ "t2")] _ _ () 
startTwoTimersReq timeoutSec1 timeoutSec2 = do
  invoke #t1  (StartTimer timeoutSec1)  
  invoke #t2  (StartTimer timeoutSec2)

--testApp = do
--  twoTimersRes
--  startTwoTimersReq 5 4


-- runAsyncApp $ MkApp (compositeHello 1 1)

compositeHello :: Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ () --'(('[CrTimers]),'[]) _ () -- '(('[()]),'[]) _ () -- AsyncTransApp m _ _ -- CompletedRes (TimerHelloFuncNicer m)
compositeHello timeoutSec1 timeoutSec2 = do
  liftIO $ putStrLn ("Entered compositeHello"::Text)
  newRes #c1 (CrRes twoTimersRes)  
  newRes #c2 (CrRes twoTimersRes)  
  invoke #c1 (CrReq $ startTwoTimersReq timeoutSec1 timeoutSec2)  
  invoke #c2 (CrReq $ startTwoTimersReq timeoutSec2 timeoutSec1)  
  pumpEvents 
  --clear #c1 
  --clear #c2
  clearAllResources 


type CrTimers = StCr (TimerArmed TaskQ "t1", TimerArmed TaskQ "t2") "c1"  
-}


