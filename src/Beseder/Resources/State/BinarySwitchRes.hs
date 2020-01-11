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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.Resources.State.BinarySwitchRes 
  ( IsBinSwitchOff
  , IsBinSwitchOn
  , BinarySwitchProv (..)
  , StBinSwitchOn
  , StBinSwitchOff
  , TurnOn (..)
  , TurnOff (..)
  ) where

import           Protolude  
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
--

data TurnOn = TurnOn deriving Show
data TurnOff = TurnOff deriving Show

class Monad m => BinarySwitchProv m res where
  data  BinSwitchOn m res 
  data  BinSwitchOff m res 
  data  ResPar m res 

  createBinarySwitch :: MkResDef m (ResPar m res) (BinSwitchOff m res)
  turnOffBinarySwitch :: RequestDef m TurnOff (BinSwitchOn m res) '[BinSwitchOff m res]  
  turnOnBinarySwitch :: RequestDef m TurnOn (BinSwitchOff m res) '[BinSwitchOn m res]  
  termOnBinarySwitch :: TermDef m (BinSwitchOn m res)  
  termOffBinarySwitch :: TermDef m (BinSwitchOff m res)  

buildRes ''BinarySwitchProv

