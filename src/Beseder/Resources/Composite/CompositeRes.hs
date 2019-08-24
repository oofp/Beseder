{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module  Beseder.Resources.Composite.CompositeRes 
  ( StCr
  , CrReq (..)
  , CrRes (..)
  ) where

import            Protolude hiding (First)
import            GHC.TypeLits (Symbol, ErrorMessage (..), TypeError)
import            Control.Concurrent.STM.TVar
import            Haskus.Utils.Variant 
import            Beseder.Base.Base
import            Beseder.Base.Internal.SplitOps
import            Beseder.Base.Internal.Core
import            Beseder.Base.Internal.STrans
import            Beseder.Base.Internal.STransFunc
import            Beseder.Base.Internal.Named
import            Beseder.Base.Internal.STransFunc
import            Beseder.Base.Internal.TypeExp
import            Control.Monad.Identity (IdentityT, runIdentityT)
import            qualified GHC.Show (Show (..))


newtype Cr a = Cr a

type StCr a name = St (Cr a) name

type instance StateTrans (StCr a name) = StateTrans a

newtype CrRes m initState sfunc = CrRes (STrans IdentityT m NoSplitter '[()] '(('[initState]),'[]) sfunc ())
instance Show (CrRes m initState sfunc) where
  show _ = "CreateComposite"

instance 
    ( Monad m
    ) => MkRes m (CrRes m initState sfunc)  where
    type ResSt m (CrRes m initState sfunc) = Cr initState 
    mkRes (CrRes t) = 
      fmap (Cr . getRes) (runIdentityT $ applyTrans t NoSplitter (return (variantFromValue ())))  
  
  
getRes :: Either (V '[]) (V '[x],()) -> x
getRes (Right (v_x, ())) = variantToValue v_x  

getResults :: Either (V '[]) (V rs,()) ->  V rs
getResults (Right (v_rs, ())) = v_rs  

data CrReq m x sfunc = CrReq (STrans IdentityT m NoSplitter '[x] '(First (Eval (sfunc NoSplitter '[x])),'[]) sfunc ())
instance Show (CrReq m x sfunc) where
  show _ = "InvokeComposite"

--data CrReq m sfunc = CrReq (forall x. STrans IdentityT m NoSplitter '[x] '(First (Eval (sfunc NoSplitter '[x])),'[]) sfunc ())
--data CrReq m sfunc where
--  MkCrReq :: forall x rs m sfunc. STrans IdentityT m NoSplitter '[x] '(rs,'[]) sfunc () -> CrReq m sfunc 

type family StCrList (rs :: [*]) (name ::Symbol) where
    StCrList '[] name = '[]
    StCrList (x ': xs) name = St (Cr x) name ': StCrList xs name

class MkStCrVar (name :: Symbol) (xs :: [*]) where
  mkStCrVar :: Named name -> V xs -> V (StCrList xs name)  

instance  MkStCrVar name '[] where
  mkStCrVar _ _ = undefined

stCrWithName :: Named name -> x -> StCr x name
stCrWithName named x = St (Cr x)

instance  
  ( MkStCrVar name xs
  , Liftable '[St (Cr x) name] (StCrList (x ': xs) name)
  , Liftable (StCrList xs name) (StCrList (x ': xs) name)
  ) => MkStCrVar name (x ': xs) where
  mkStCrVar named v = case popVariantHead v of
    Right x -> liftVariant (variantFromValue (stCrWithName named x))
    Left v_xs -> liftVariant (mkStCrVar named v_xs) 
  
instance 
  ( Monad m
  , xs ~ (First (Eval (sfunc NoSplitter '[x])))
  , MkStCrVar name xs
  ) => Request m (CrReq m x sfunc) (StCr x name) where
  type ReqResult (CrReq m x sfunc) (StCr x name) = StCrList (First (Eval (sfunc NoSplitter '[x]))) name 
  request (CrReq t) (St (Cr x))=
    let named :: Named name
        named = Named  
    in fmap (mkStCrVar named . getResults) (runIdentityT $ applyTrans t NoSplitter (return (variantFromValue x)))  

instance  
  ( MonadIO m
  , ExtendStateTrans x
  , StateTrans x ~ 'Dynamic
  , Transition m (ExtendedStateTrans x)
  , MkStCrVar name (NextStates (ExtendedStateTrans x))
  ) => Transition m  (StCr x name) where
  type NextStates (StCr x name) = StCrList (NextStates (ExtendedStateTrans x)) name 
  next (St (Cr x)) cb = 
    let named :: Named name
        named = Named  
    in next (extendStateTrans x) (\v_next -> cb (mkStCrVar named v_next))


instance  
  ( Monad m
  , TermState m x
  ) => TermState m  (StCr x name) where
    terminate (St (Cr x)) = terminate x
  