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

itResTrans :: Int -> Int -> STrans IdentityT TaskQ NoSplitter '[()] _ _ ()
itResTrans to1 to2 = do 
  newRes #t1 TimerRes >> newRes #t2 TimerRes
  invoke #t1 (StartTimer to1)
  invoke #t2 (StartTimer to2)

type Hnd 
  =   On ("t1" :? IsTimerTriggered) 
        (   PutStrLn "Triggered t1" 
        :>> ClearResource "t1"
        :>> NewRes "t1" TimerRes TaskQ
        :>> "to1" |> Invoke "t1" StartTimer
        )
  :>> On ("t2" :? IsTimerTriggered) 
       ( PutStrLn "Triggered t2"
       :>> ClearResource "t2"
       :>> NewRes "t2" TimerRes TaskQ
       :>> "to2" |> Invoke "t2" StartTimer
       )


dict to1 to2 
  = Patches  
    ( CnP (StartTimer to1) `as` #to1,
    ( CnP (StartTimer to2) `as` #to2))
    
twoTimerRes :: Int -> Int -> CrResH TaskQ _ _ (Hnd) _
twoTimerRes to1 to2 = CrResH (itResTrans to1 to2) (dict to1 to2) 

type StopBothTimers = 
  Invoke "t1" StopTimer
  :>> Invoke "t2" StopTimer
  
crReqStopBoth :: CrReqF TaskQ  StopBothTimers  
crReqStopBoth = CrReqF

twoTimersApp :: Int -> Int -> Int -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ ()
twoTimersApp to1 to2 toAll = do
  newRes #tt (twoTimerRes to1 to2)
  newRes #tAll TimerRes
  invoke #tAll (StartTimer toAll)
  try @("tAll" :? IsTimerArmed) pumpEvents
  invoke #tt crReqStopBoth
  clearAllResources

--runAsyncApp $ MkApp (twoTimersApp 3 4 30)
