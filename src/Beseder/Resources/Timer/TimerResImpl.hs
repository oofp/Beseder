{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Beseder.Resources.Timer.TimerResImpl where

import           Protolude    
import           Control.Concurrent.STM.TVar
import           Control.Monad.Cont
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.Timer.TimerRes

instance TaskPoster m => TimerProv m where
  data TimerNotArmedEvData m = TimerNotArmedData  
  data TimerArmedEvData m =  TimerArmedData (Async ()) (TVar (Maybe (TimerTriggeredEvData m -> m Bool)))
  data TimerTriggeredEvData m = TimerTriggeredData 
  data TimerStoppedEvData m = TimerStoppedData
  
  createTimer TimerRes = return TimerNotArmedData
  startTimer (StartTimer timeoutSec) TimerNotArmedData = do
    taskPoster <- getTaskPoster
    cbTvar <- liftIO $ newTVarIO Nothing
    task <- liftIO $ async $ do
      threadDelay (timeoutSec*1000000)
      taskPoster $ do
        cbMaybe <- liftIO $ atomically $ readTVar cbTvar 
        case cbMaybe of
          Nothing -> putStrLn ("TimerProvImpl: callback not found" :: Text) >> return True
          Just cbFunc -> cbFunc TimerTriggeredData 
    return $ TimerArmedData task cbTvar      

  stopTimer StopTimer (TimerArmedData asyncTask cbTvar) = 
    liftIO $ do
      putStrLn ("TimerProvImpl: stop timer" :: Text)
      cancel asyncTask
      atomically $ writeTVar cbTvar Nothing
      return TimerStoppedData
  clearNotArmedTimer TimerNotArmedData = return ()
  clearStoppedTimer TimerStoppedData = return ()
  clearTriggeredTimer TimerTriggeredData = return ()
  timerTransition (TimerArmedData asyncTask cbTvar) cbFunc  = 
    liftIO $ atomically $ writeTVar cbTvar (Just cbFunc) >> return True


    