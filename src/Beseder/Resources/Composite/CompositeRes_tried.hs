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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module  Beseder.Resources.Composite.CompositeRes_tried 
  ( EvHnd
  , EvHandler (..)
  , compRes
  , CResPar
  , CompReq  
  ) where

import            Protolude hiding (First)
import            GHC.TypeLits (Symbol, ErrorMessage (..), TypeError)
import            Haskus.Utils.Variant 
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Internal.SplitOps
import            Beseder.Base.Internal.Core
import            Beseder.Base.Internal.StHelper
import            Beseder.Base.Internal.STransIx
import            Beseder.Base.Internal.TypeExp
import            Control.Monad.Identity (IdentityT, runIdentityT)

-- type EvHnd f m = forall xs ys. Eval (f NoSplitter xs) ~ '(ys, '[]) => STrans IdentityT m NoSplitter xs ys '[] f ()
--type EvHnd f m = forall xs rs ex. Eval (f NoSplitter xs) ~ '(rs, ex) => STrans IdentityT m NoSplitter xs rs ex f ()
type EvHnd f m = forall xs. STrans IdentityT m NoSplitter xs (First (Eval (f NoSplitter xs))) '[] f ()
--type EvHnd f m = forall xs rs ex. Eval (f NoSplitter xs) ~ '(rs, ex) => STrans IdentityT m NoSplitter xs rs ex f ()

data EvHandler f m where
  MkHandler ::  Eval (f NoSplitter xs) ~ '(rs, ex) => STrans IdentityT m NoSplitter xs rs ex f () -> EvHandler f m

--data EvHandler f m (cx :: [*] -> Constraint) where
--  MkHandler :: cx xs => STrans IdentityT m NoSplitter xs (First (Eval (f NoSplitter xs))) '[] f () -> EvHandler f m cx

data CResPar f m res = CResPar (STransApp IdentityT m NoSplitter '[()] '[res] '[] ()) (EvHnd f m) 
data CRes f m res  = CRes (EvHnd f m) res 

compRes :: STransApp IdentityT m NoSplitter '[()] '[res] '[] () -> EvHnd f m -> CResPar f m res
compRes = CResPar

instance 
  ( Monad m
  ) => MkRes m (CResPar f m res)  where
  type ResSt m (CResPar f m res ) = CRes f m res  
  mkRes (CResPar (MkApp (STrans t)) tr) = 
    fmap (getRes tr) (runIdentityT $ t NoSplitter (variantFromValue ()))   

extractRes :: Either (V ex) (V rs, a) -> V rs
extractRes (Right (v,_)) = v

getRes :: EvHnd f m -> Either (V '[]) (V '[x],()) -> CRes f m x 
getRes tr ei = CRes tr (variantToValue (extractRes ei))  

applyHnd :: (Monad m) => V xs -> EvHnd f m -> m (V (First (Eval (f NoSplitter xs))))
applyHnd v_xs (STrans t) = fmap extractRes (runIdentityT $ t NoSplitter v_xs) 

--

type StCs res f m name = St (CRes f m res) name  
type instance StateTrans (StCs res f m name) = StateTrans res

instance 
  ( Transition m res
  , StateTrans (StCs res f m name) ~ 'Dynamic
  , StWrapVar (CRes f m) (First (Eval (f NoSplitter (NextStates res)))) name
  ) => Transition m (StCs res f m name) where
  type NextStates (StCs res f m name) = StWrapList (CRes f m) name (First (Eval (f NoSplitter (NextStates res))))
  next (St (CRes t res)) cb = 
    next res (\v_next -> 
      do
        v_ys <- applyHnd v_next t  
        cb (asStWrapVar (CRes t) (Named @name) v_ys))

type CompReq m res xs = STransApp IdentityT m NoSplitter '[res] xs '[] ()

applyHnd' :: (Monad m) => res -> CompReq m res xs -> m (V xs)
applyHnd' res (MkApp (STrans t)) = fmap extractRes (runIdentityT $ t NoSplitter (variantFromValue res)) 

instance 
  ( Monad m 
  , StWrapVar (CRes f m) xs name
  ) => Request m (CompReq m res xs) (StCs res f m name) where
  type ReqResult (CompReq m res xs) (StCs res f m name) = StWrapList (CRes f m) name xs 
  request treq (St (CRes t res)) = do 
    v_ys <- applyHnd' res treq
    return (asStWrapVar (CRes t) (Named @name) v_ys)

instance  
  ( Monad m
  , TermState m res
  ) => TermState m (StCs res f m name) where
    terminate (St (CRes t res)) = terminate res 


