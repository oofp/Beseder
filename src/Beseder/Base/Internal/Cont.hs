{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}


module Beseder.Base.Internal.Cont where

import           Control.Monad.Cont
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Protolude                    hiding (Product, handle)
import           Beseder.Base.Internal.TupleHelper

type ContQ (m :: * -> *) res = ContT Bool m res 

contW ::(Transition m state1) => state1 -> ContQ m (V (NextStates state1))
contW state1 = ContT (next state1)

contW' ::(ExtendStateTrans state1, Transition m (ExtendedStateTrans state1)) => state1 -> ContQ m (V (NextStates (ExtendedStateTrans state1)))
contW' state1 = ContT (next (extendStateTrans state1))

contR ::(Request m req state1, MonadTrans q) => req -> state1 -> q m (V (ReqResult req state1))
contR req state1 = lift (request req state1)

contL :: (MonadTrans q, Monad (q m)) => q m a -> state1 -> q m (state1,a)
contL qa state1 =  do
  a <- qa 
  return (state1,a) 

contC ::(ClearableState m (TargetByName name state1), TT state1 (TargetByName name state1), MonadTrans q) => Named name -> state1 -> q m (ClearResult (TargetByName name state1))
contC named state1 = lift $ clearState (transformForReq (byName named state1))

contClearVar ::(TermState m (V xs), MonadTrans q) => V xs -> q m (V '[()])
contClearVar v_xs = lift $ (variantFromValue <$> (terminate v_xs))

contClearAllVar ::(TermState m (ClrVar xs), MonadTrans q) => V xs -> q m (V '[()])
contClearAllVar v_xs = lift $ (variantFromValue <$> (terminate (ClrVar v_xs)))

contRes ::(KnownSymbol name, CreateRes m name res state, MonadTrans q) => Named name-> res -> q m state
contRes named res = lift $ createRes named res

