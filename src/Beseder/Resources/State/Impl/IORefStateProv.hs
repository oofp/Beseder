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

module Beseder.Resources.State.Impl.IORefStateProv 
  ( ioRefStateRes
  ) where

import           Protolude    
import           GHC.Show (Show (..))
import           Data.IORef
import           Data.Coerce
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.State.MonoStateProv

newtype IORefState s = IORefState s deriving Show

ioRefStateRes :: s -> IORefStateRes s
ioRefStateRes =  coerce

type IORefStateRes s = StateRes IORefState s
instance MonadIO m => MonoStateProv IORefState s m where
  data MonoStateData IORefState s m = IORefData (IORef s) 
  
  createState  (StateRes (IORefState s))  = liftIO (IORefData <$> newIORef s)

  setState  (SetState s) st@(IORefData ioRef) = liftIO $ writeIORef ioRef s >> return st
  modifyState (ModifyState f) st@(IORefData ioRef) = liftIO $ modifyIORef ioRef f >> return st

  clearState _ = return ()

  getDataState (IORefData ioRef) = liftIO $ readIORef ioRef

