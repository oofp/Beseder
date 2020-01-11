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
  ) where

import           Protolude  
import           Haskus.Utils.Variant
-- import           qualified GHC.Show (Show (..))
import           Beseder.Misc.Prosumers.Consumer
import           Beseder.Resources.State.BinarySwitchRes
--

data BinSwitchCons cons = BinSwitchCons cons
instance (Monad m, GetConsumer cons m Bool) => BinarySwitchProv m (BinSwitchCons cons) where
  data  BinSwitchOn m (BinSwitchCons cons) = BinSwitchOn (Consumer m Bool)  
  data  BinSwitchOff m (BinSwitchCons cons) = BinSwitchOff (Consumer m Bool)
  data  ResPar m (BinSwitchCons cons) = MkBinSwitchCons cons 

  createBinarySwitch (MkBinSwitchCons consSrc) = BinSwitchOff <$> consumer consSrc

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

