{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Beseder.Base.Internal.NatOne where

import Protolude
import GHC.TypeLits

data NatOne
    = One
    | Succ (NatOne)


type family NatConv (n :: Nat) :: NatOne where
    NatConv n = NatConv' n 1 One 

type family NatConv' (n :: Nat) (n_c :: Nat) (n1 :: NatOne) :: NatOne where
    NatConv' n0 n0 n1 = n1
    NatConv' n c n1 = NatConv' n (c + 1) (Succ n1)
      
    
