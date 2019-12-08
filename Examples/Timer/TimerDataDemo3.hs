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

module  TimerDataDemo3 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

helloFlow3 :: STransData TaskQ NoSplitter _ _
helloFlow3 = do
  newRes #t TimerRes
  invoke #t (StartTimer 36)
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 3)
  --try @("t" :? IsTimerArmed) $ do
  handleEvents $ do
      label #handleEvents 
      liftIO $ putStrLn ("handleEvents ...." :: Text)
      on @("t1" :? IsTimerTriggered) $ do 
        label #atOn 
        clear #t1
        newRes #t1 TimerRes
        invoke #t1 (StartTimer 3)
  liftIO $ putStrLn ("loop is completed" :: Text)
  termAndClearAllResources  

  
runFlow :: IO ()  
runFlow = runAsyncData helloFlow3

-- :t evalSTransDataNamedLabels #atOn helloFlow3
-- :t evalSTransDataAppFiltered (Proxy @(MatchFunc LiftIOFunc))  helloFlow3
-- :t flattenSteps $ evalSTransDataLabels'  helloFlow3 (Proxy :: Proxy '[()])