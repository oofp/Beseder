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

--module Providers.Impl.STMStateProv 
module Beseder.Resources.State.Impl.STMStateProv 
  ( stmStateRes
  ) where

import           Protolude    
import           GHC.Show (Show (..))
import           Control.Concurrent.STM.TVar
import           Data.Coerce
import           Beseder.Resources.State.MonoStateProv

newtype STMState s = STMState s deriving Show

stmStateRes :: s -> STMStateRes s
stmStateRes =  coerce

type STMStateRes s = StateRes STMState s
instance MonadIO m => MonoStateProv STMState s m where
  data MonoStateData STMState s m = STMData (TVar s) 
  
  createState  (StateRes (STMState s))  = liftIO (STMData <$> newTVarIO s)

  setState  (SetState s) st@(STMData tvar) = liftIO $ atomically (writeTVar tvar s) >> return st
  modifyState (ModifyState f) st@(STMData tvar) = liftIO $ atomically $ modifyTVar tvar f >> return st

  clearState _ = return ()

  getDataState (STMData ioRef) = liftIO $ readTVarIO ioRef

--
