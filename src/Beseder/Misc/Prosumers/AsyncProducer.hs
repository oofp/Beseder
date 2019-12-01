{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor          #-}

module Beseder.Misc.Prosumers.AsyncProducer where

import           Protolude
import           Beseder.Misc.Prosumers.Producer
import           Control.Concurrent.STM.TVar
import           Beseder.Base.Common

-- data AsyncProducer a = AsyncProducer (Async ()) (TVar (Maybe (a -> m ()))) 

initAsyncProducer :: TaskPoster m => IO a -> m (Producer m a)
initAsyncProducer action = do
  taskPoster <- getTaskPoster
  cbTvar <- liftIO $ newTVarIO Nothing
  asyncTask <- liftIO $ async $ forever $ do
    a <- action
    taskPoster $ do
      cbMaybe <- liftIO $ atomically $ readTVar cbTvar 
      case cbMaybe of
        Nothing -> putStrLn ("initAsyncProducer: callback not found" :: Text) 
        Just cbFunc -> cbFunc a  
      return True 
  let 
    theProducer = Producer 
      { produce = \cbMaybe -> do
          liftIO $ atomically $ writeTVar cbTvar cbMaybe
          when (isNothing cbMaybe) (liftIO $ cancel asyncTask)
      }
  return theProducer      

initAsyncProducer2 :: TaskPoster m => (Maybe a -> IO a)  -> m (Producer m a)
initAsyncProducer2 action = do
  taskPoster <- getTaskPoster
  cbTvar <- liftIO $ newTVarIO Nothing
  let 
    go aMaybe = do
      a <- action aMaybe
      -- putStrLn ("initAsyncProducer: going to post event" :: Text) 
      taskPoster $ do
        cbMaybe <- liftIO $ atomically $ readTVar cbTvar 
        case cbMaybe of
          Nothing -> putStrLn ("initAsyncProducer: callback not found" :: Text) 
          Just cbFunc -> do 
            -- putStrLn ("initAsyncProducer: invoking callback" :: Text) 
            cbFunc a  
        return True 
      go (Just a)
  asyncTask <- liftIO $ async $ go Nothing
  let
    theProducer = Producer 
      { produce = \cbMaybe -> do
          liftIO $ atomically $ writeTVar cbTvar cbMaybe
          when (isNothing cbMaybe) (liftIO $ cancel asyncTask)
      }
  return theProducer      
  