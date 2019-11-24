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

module  TimerDemo3 where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 

helloFlow3 ::AsyncApp TaskQ '[()] _ _
helloFlow3 = MkApp $ do
  newRes #t TimerRes
  invoke #t (StartTimer 36)
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 3)
  try @("t" :? IsTimerArmed) $ do
    handleEvents $ do
      liftIO $ putStrLn ("handleEvents ...." :: Text)
      on @("t1" :? IsTimerTriggered) $ do 
        clear #t1
        newRes #t1 TimerRes
        invoke #t1 (StartTimer 3)
  liftIO $ putStrLn ("loop is completed" :: Text)
  termAndClearAllResources  

runFlow :: IO ()  
runFlow = runAsyncApp helloFlow3
    
  