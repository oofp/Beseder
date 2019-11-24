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

module Beseder.Resources.State.BinarySwitch 
  ( BinSwitchOn
  , BinSwitchOff
  , IsBinSwitchOff
  , IsBinSwitchOn
  , TurnOn (..)
  , TurnOff (..)
  , binSwRes
  ) where

import           Protolude  
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           qualified GHC.Show (Show (..))
import           Beseder.Utils.BoolHelper 
import           Beseder.Resources.State.ImpRes
import           Beseder.Misc.Prosumers.Consumer
--

newtype BinarySwitchRes (m :: * -> *)  cons = BinarySwitchRes cons 
newtype BinarySwitch (fl :: Bool) m = BinarySwitch (Consumer m Bool) 

binSwRes :: (GetConsumer cons m Bool, Monad m) => cons -> m (ImpRes (BinarySwitchRes m cons))
binSwRes = return . ImpRes . BinarySwitchRes

instance Show (BinarySwitchRes m cons) where show _ = "BinarySwitchRes"

data TurnOn = TurnOn deriving Show
data TurnOff = TurnOff deriving Show

instance (Monad m, GetConsumer cons m Bool) => MkImpRes m (BinarySwitchRes m cons) where
  type ImpResInitState m (BinarySwitchRes m cons) = (BinarySwitch 'False m)
  mkImpRes (BinarySwitchRes cons) =  
    BinarySwitch <$> consumer cons     

toggleSwitch :: (Monad m , notFl ~ NotBool fl, KnownBool notFl) => Proxy notFl -> BinarySwitch fl m -> m (V '[BinarySwitch (NotBool fl) m])
toggleSwitch px (BinarySwitch c@(Consumer cfunc))  = do
  let bVal = boolVal px
  cfunc (Just bVal)
  return $ variantFromValue (BinarySwitch c)

instance (Monad m) => ImpOp m TurnOn (BinarySwitch 'False m) where
  type OpImpResults TurnOn (BinarySwitch 'False m) = '[BinarySwitch 'True m]
  opImpReq :: TurnOn -> BinarySwitch 'False m -> m (V '[BinarySwitch 'True m])
  opImpReq TurnOn = toggleSwitch (Proxy :: Proxy True)
  
instance (Monad m) => ImpOp m TurnOff (BinarySwitch 'True m) where
  type OpImpResults TurnOff (BinarySwitch 'True m) = '[BinarySwitch 'False m]
  opImpReq TurnOff = toggleSwitch (Proxy :: Proxy False)
    
type BinSwitchOn m name = St (ImpSt (BinarySwitch 'True m )) name 
type BinSwitchOff m name = St (ImpSt (BinarySwitch 'False m)) name 
  
data IsBinSwitchOn :: Type -> Exp Bool 
type instance Eval (IsBinSwitchOn a) = IsBinSwitchOnFam a
data IsBinSwitchOff :: Type -> Exp Bool 
type instance Eval (IsBinSwitchOff a) = IsBinSwitchOffFam a

type family IsBinSwitchOnFam a :: Bool where
  IsBinSwitchOnFam (BinSwitchOn _ _) = 'True
  IsBinSwitchOnFam _ = 'False

type family IsBinSwitchOffFam a :: Bool where
  IsBinSwitchOffFam (BinSwitchOff _ _) = 'True
  IsBinSwitchOffFam _ = 'False
  