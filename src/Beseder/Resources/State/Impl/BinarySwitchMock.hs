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

module Beseder.Resources.State.Impl.BinarySwitchMock 
  ( BinarySwitchMock (..)
  , BinarySwitchMockRes
  , mkBinarySwitchMock 
  ) where
  
import           Protolude 
import           Beseder.Base.Internal.Classes 
import           Beseder.Resources.Utils.ResourceMock
import           Beseder.Resources.State.BinarySwitchRes

data BinarySwitchMock = BinaryMonitorMock
type BinarySwitchMockRes m = ResPar m BinarySwitchMock

mkBinarySwitchMock :: TimeoutRange -> BinarySwitchMockRes m
mkBinarySwitchMock  = MkBinarySwitchMock

instance (MonadIO m, TaskPoster m) => BinarySwitchProv m BinarySwitchMock where
  newtype  BinSwitchOn m BinarySwitchMock = BinarySwitchOn (MockStateData m) deriving (HasMockState m)  
  newtype  BinSwitchOff m BinarySwitchMock = BinarySwitchOff (MockStateData m) deriving (HasMockState m)  
  newtype  ResPar m BinarySwitchMock = MkBinarySwitchMock TimeoutRange 

  createBinarySwitch (MkBinarySwitchMock toRange) = newMockState toRange 

  turnOnBinarySwitch TurnOn = handleRequest  
  turnOffBinarySwitch TurnOff = handleRequest
  termOnBinarySwitch = releaseMockState
  termOffBinarySwitch = releaseMockState

