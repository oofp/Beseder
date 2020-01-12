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

module Beseder.Resources.State.Impl.BinarySwitchCons 
  ( BinSwitchCons (..)
  , mkBinSwitchCons
  , BinSwitchConsRes
  ) where

import           Protolude  
import           Haskus.Utils.Variant
-- import           qualified GHC.Show (Show (..))
import           Beseder.Misc.Prosumers.Consumer
import           Beseder.Resources.State.BinarySwitchRes
--

data BinSwitchCons = BinSwitchCons 
type BinSwitchConsRes m = ResPar m BinSwitchCons

mkBinSwitchCons :: Consumer m Bool -> BinSwitchConsRes m
mkBinSwitchCons = MkBinSwitchCons

instance Monad m => BinarySwitchProv m BinSwitchCons where
  data  BinSwitchOn m BinSwitchCons = BinSwitchOn (Consumer m Bool)  
  data  BinSwitchOff m BinSwitchCons = BinSwitchOff (Consumer m Bool)
  data  ResPar m BinSwitchCons = MkBinSwitchCons (Consumer m Bool) 

  createBinarySwitch (MkBinSwitchCons cons) = return $ BinSwitchOff cons

  turnOnBinarySwitch TurnOn (BinSwitchOff cons) = do 
    (consume cons) (Just True) 
    return $ variantFromValue $ BinSwitchOn cons
  turnOffBinarySwitch TurnOff (BinSwitchOn cons) = do 
    (consume cons) (Just False) 
    return $ variantFromValue $ BinSwitchOff cons

  termOnBinarySwitch (BinSwitchOn cons) = (consume cons) Nothing   
  termOffBinarySwitch (BinSwitchOff cons) = (consume cons) Nothing   

--binSwRes :: (GetConsumer cons m Bool, Monad m) => cons -> m (ImpRes (BinarySwitchRes m cons))
--binSwRes = return . ImpRes . BinarySwitchRes

