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
{-# LANGUAGE ScopedTypeVariables   #-}

module Beseder.Resources.Task.Impl.AsyncTaskProv 
  ( asyncTaskResource
  , AsyncTask
  ) where

import           Protolude    
import           Beseder.Resources.Task.TaskRes
import           Haskus.Utils.Variant
import           Control.Concurrent.STM.TVar
import           Beseder.Base.Common

data  AsyncTask a 
type AsyncTaskPar m a = ResPar m (AsyncTask a)

asyncTaskResource :: forall m a. IO a -> AsyncTaskPar m a  
asyncTaskResource = MkAsyncTask

instance TaskData (AsyncTask a) where
  type instance TaskResult (AsyncTask a) = a
  type instance TaskError (AsyncTask a) = SomeException 

instance TaskPoster m => TaskProv m (AsyncTask a) where
  data TaskInProgres m (AsyncTask a) = TaskInProgres (Async ()) (TVar (Maybe (V '[TaskCompleted m (AsyncTask a), TaskFailed m (AsyncTask a)] -> m ())))
  newtype  TaskCompleted m (AsyncTask a) = TaskCompleted a 
  newtype  TaskFailed m (AsyncTask a) = TaskFailed SomeException
  data  TaskCancelled m (AsyncTask a) = TaskCancelled
  data  ResPar m (AsyncTask a) = MkAsyncTask (IO a)

  startTask :: ResPar m (AsyncTask a) -> m (TaskInProgres m (AsyncTask a))
  startTask (MkAsyncTask io_a) = do 
    cb <- liftIO $ newTVarIO Nothing
    taskPoster <- getTaskPoster
    task <- liftIO $ async $ do
      ioRes <- try io_a
      taskPoster $ do      
        cbMaybe <- liftIO $ atomically $ readTVar cb
        case cbMaybe of 
          Nothing -> return True
          Just cbFunc -> do 
            liftIO $ atomically $ writeTVar cb Nothing  
            fireCompletion cbFunc ioRes
            return True
    return (TaskInProgres task cb)

  cancelTask CancelTask (TaskInProgres asyncTask cb) = do 
    liftIO $ atomically $ writeTVar cb Nothing 
    liftIO $ cancel asyncTask
    return (variantFromValue TaskCancelled)


  taskTransition (TaskInProgres _asyncTask cb) cbFunc = 
    liftIO $ atomically $ writeTVar cb (Just cbFunc) 

  termCompleted (TaskCompleted _a) = return ()
  termCancelled TaskCancelled = return ()
  termFailed (TaskFailed _e) = return ()

  _taskResult (TaskCompleted a) = return a
  _taskError (TaskFailed e) = return e



fireCompletion :: (V '[TaskCompleted m (AsyncTask a), TaskFailed m (AsyncTask a)] -> m ()) -> Either SomeException a -> m ()
fireCompletion cbFunc (Right a) = cbFunc (toVariantAt @0 (TaskCompleted a)) 
fireCompletion cbFunc (Left e) = cbFunc (toVariantAt @1 (TaskFailed e)) 
