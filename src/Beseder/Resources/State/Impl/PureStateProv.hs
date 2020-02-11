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

--module Providers.Impl.PureStateProv 
module Beseder.Resources.State.Impl.PureStateProv 
  ( pureStateRes
  ) where

import           Protolude    
import           GHC.Show (Show (..))
import           Data.Coerce
import           Beseder.Resources.State.MonoStateProv
    
newtype PureState s = PureState s deriving Show

pureStateRes :: s -> PureStateRes s
pureStateRes =  coerce

type PureStateRes s = StateRes PureState s
instance Monad m => MonoStateProv PureState s m where
  data MonoStateData PureState s m = PureData s 
  
  createState  (StateRes (PureState s))  = return (PureData s)

  setState  (SetState s) _ = return (PureData s)
  modifyState (ModifyState f) (PureData s) = return (PureData (f s))

  clearState _ = return ()

  getDataState (PureData s) = return s

--
