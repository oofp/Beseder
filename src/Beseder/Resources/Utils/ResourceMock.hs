{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beseder.Resources.Utils.ResourceMock
  ( MockStateData
  , TimeoutRange
  , HasMockState
  , newMockState 
  , setMockStateListener 
  , handleRequest 
  , releaseMockState
  ) where
  
import           Protolude 
import           Haskus.Utils.Variant
import           Haskus.Utils.Types.List
import           Control.Concurrent.STM.TVar
import           Beseder.Base.Internal.Classes 
import           Data.Coerce (coerce)
import           Beseder.Utils.VariantHelper
import           System.Random

type TimeoutRange = (Int,Int) 

data MockStateData m 
  = MockStateData 
      { cb :: TVar (Maybe (m ()))
      , asyncTask :: Async ()
      , timeoutRange :: TimeoutRange
      }

initMockStateData :: (MonadIO m, TaskPoster m) => TimeoutRange -> m (MockStateData m)
initMockStateData fromToSec = do 
  timeoutSec <- liftIO $ randomRIO fromToSec
  pst <- getTaskPoster 
  currentCb <- liftIO $ newTVarIO Nothing  
  asTask <- liftIO $ async $ do
    threadDelay (timeoutSec*1000000)
    liftIO $ putStrLn (("initMockStateData: Delay completed; Timeout:"::Text) <> show timeoutSec) 
    pst $ do
      taskMaybe <- liftIO $ atomically $ readTVar currentCb
      case taskMaybe of 
        Just task -> task
        Nothing -> do 
          liftIO $ putStrLn (("initMockStateData: Race condition detected; Timeout:"::Text) <> show timeoutSec) 
          return ()
      return True  
  return $ 
    MockStateData
      { cb = currentCb
      , asyncTask = asTask
      , timeoutRange = fromToSec
      }

releaseMockStateData :: MonadIO m => MockStateData m -> m ()
releaseMockStateData d = liftIO $ do 
  liftIO $ atomically $ writeTVar (cb d) Nothing
  cancel (asyncTask d)

setMockStateDataListener :: MonadIO m => MockStateData m -> m () -> m ()
setMockStateDataListener d lst = liftIO $ atomically $ writeTVar (cb d) (Just lst) 

unifyCallback :: 
  ( KnownNat (Length nextStates)
  , MonadIO m
  , GetVarInstanceFrom (MockStateData m) nextStates
  ) => ((V nextStates) -> m ()) -> (MockStateData m -> m ())
unifyCallback cbFunc stData = getRandomVarFrom stData >>= cbFunc  

taskForCallback :: 
  ( KnownNat (Length nextStates)
  , MonadIO m
  , GetVarInstanceFrom (MockStateData m) nextStates
  , TaskPoster m
  ) => TimeoutRange -> ((V nextStates) -> m ()) -> m ()
taskForCallback toRange cbFunc = initMockStateData toRange >>= unifyCallback cbFunc

class HasMockState m a where
  getMockState :: a -> m (MockStateData m)

instance Monad m => HasMockState m (MockStateData m) where
  getMockState ms = return ms

handleRequest :: 
  ( TaskPoster m, KnownNat (Length xs)
  ,  HasMockState m st
  , GetVarInstanceFrom (MockStateData m) xs
  ) => st -> m (V xs)
handleRequest st = do
  d <- getMockState st
  releaseMockStateData d
  initMockStateData (timeoutRange d) >>= getRandomVarFrom

newMockState :: (TaskPoster m, Coercible st (MockStateData m)) => TimeoutRange -> m st
newMockState toRange = fmap coerce (initMockStateData toRange)

setMockStateListener :: (HasMockState m st,
                          KnownNat (Length nextStates),
                          GetVarInstanceFrom (MockStateData m) nextStates, TaskPoster m) =>
                        st -> (V nextStates -> m ()) -> m ()
setMockStateListener st lstner = do
  d <- getMockState st
  setMockStateDataListener d (taskForCallback (timeoutRange d) lstner)

releaseMockState :: (HasMockState m a, MonadIO m) => a -> m ()
releaseMockState st = 
  getMockState st >>= releaseMockStateData
