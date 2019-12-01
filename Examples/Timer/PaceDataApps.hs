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
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  PaceDataApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
--import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           Beseder.Base.ControlData 
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)

paceHelloData :: forall m. Int -> STransData m NoSplitter _ () 
paceHelloData timeoutSec1 = do
  newRes #t1 TimerRes
  invoke #t1  (StartTimer timeoutSec1)
  newRes #pace (PaceResPar :: ResPar m PaceRes)
  invoke #pace (StartPace 1)
  try @("t1" :? IsTimerArmed) $ do
    handleEvents $ do
      liftIO $ putStrLn ("pacing" :: Text)
  invoke #pace StopPace    
  clearAllResources 


--paceHello :: (TaskPoster m) => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
--paceHello timeoutSec1 = interpret (paceHelloData timeoutSec1)

runHelloPace :: IO ()  
runHelloPace = runAsyncData $ (paceHelloData 10)
