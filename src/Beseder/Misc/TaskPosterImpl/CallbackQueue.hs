{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}

module Beseder.Misc.TaskPosterImpl.CallbackQueue where

import           Control.Concurrent.STM.TChan
import           Protolude
import           Beseder.Base.Common

class  HasCallbackChan q r where
  getChan :: r -> TChan (q Bool)

instance (MonadIO m, MonadReader r m, HasCallbackChan m r) => TaskPoster m where
  getTaskPoster = do
    chan <- asks getChan
    return (atomically . writeTChan chan)

runQueue :: (MonadIO m, MonadReader r m, HasCallbackChan m r) => m ()
runQueue = do
    chan <- asks getChan
    go chan 
  where
    go chan = do
      task <- liftIO $ atomically $ readTChan chan  
      flContinue <- task
      when flContinue (go chan)

