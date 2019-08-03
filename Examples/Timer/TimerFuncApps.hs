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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerApps where


import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont

instance GetInstance StartTimer where
  getInstance = StartTimer 5

type TimerBasicFunc m = 
  NewResFunc TimerRes "t1" m 
  :>> InvokeAllFunc StartTimer "t1" 
  :>> GetNextAllFunc 
  :>> ClearAllFunc "t1"  
-- :kind! EvalTransFunc IO TimerEmptyFunc

buildTrans :: ToTrans (TimerBasicFunc m) q m NoSplitter xs => STrans q m NoSplitter xs _ (TimerBasicFunc m) ()
buildTrans =
  let px_t :: Proxy (TimerBasicFunc m)
      px_t = Proxy
      px_m :: Proxy m
      px_m = Proxy
      px_sp :: Proxy NoSplitter
      px_sp = Proxy
  in toTrans px_t px_m px_sp    

executableTrans :: TaskPoster m => ExcecutableTrans (ContT Bool) m (TimerBasicFunc m) 
executableTrans = buildTrans  

runTimerBasic :: IO ()
runTimerBasic = runAsyncTrans executableTrans
