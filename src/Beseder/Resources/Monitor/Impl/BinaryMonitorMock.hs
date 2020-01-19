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

module Beseder.Resources.Monitor.Impl.BinaryMonitorMock
  ( BinaryMonitorMock (..)
  , BinaryMonitorMockRes
  , mkBinaryMonitorMock 
  ) where
  
import           Protolude 
import           Beseder.Resources.Monitor.BinaryMonitorRes
import           Beseder.Base.Internal.Classes 
import           Beseder.Resources.Utils.ResourceMock

data BinaryMonitorMock = BinaryMonitorMock
type BinaryMonitorMockRes m = ResPar m BinaryMonitorMock

mkBinaryMonitorMock :: Monad m => TimeoutRange -> m (BinaryMonitorMockRes m)
mkBinaryMonitorMock  = return . MkBinaryMonitorMock

instance (MonadIO m, TaskPoster m)  => BinaryMonitorProv m BinaryMonitorMock where
  newtype  BinMonitorOn m BinaryMonitorMock = BinMonitorOn (MockStateData m) deriving (HasMockState m)
  newtype  BinMonitorOff m BinaryMonitorMock = BinMonitorOff (MockStateData m) deriving (HasMockState m)
  newtype  BinMonitorStopped m BinaryMonitorMock = BinMonitorStopped (MockStateData m) deriving (HasMockState m)
  data  ResPar m BinaryMonitorMock = MkBinaryMonitorMock TimeoutRange

  createBinaryMonitor (MkBinaryMonitorMock toRange) = newMockState toRange
  stopBinaryMonitorOn  StopMonitor = handleRequest -- st 
  stopBinaryMonitorOff StopMonitor = handleRequest -- st 
  onOffTransition = setMockStateListener -- st cb 
  offOnTransition = setMockStateListener -- st cb 
  termBinaryMonitor = releaseMockState -- st

