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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  Beseder.Resources.Composite.CompositeRes
  ( compRes
  , CResPar
  , CompReq
  , StateHandlerProv (..)  
  , StatesHandlerProv (..)
  , compReq
  , invokeC
  ) where

import            Protolude hiding (First)
import            Haskus.Utils.Variant 
import            Haskus.Utils.Types.List
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Base.Internal.SplitOps
import            Beseder.Base.Internal.Core
import            Beseder.Base.Internal.StHelper
import            Beseder.Base.Internal.STransIx
import            Control.Monad.Identity (IdentityT, runIdentityT)
import            qualified GHC.Show (Show (..))
import qualified Prelude as SafeUndef (undefined) 

type StateHandler (m :: * -> *) (hf :: * -> Exp [*])  (res :: *) = res -> m (V (Eval (hf res)))  

class StateHandlerProv m (hf :: * -> Exp ([*])) (res :: *)  where 
  getHandler :: Proxy hf -> StateHandler m hf res

class StatesHandlerProv (m :: * -> *) (hf :: * -> Exp [*])  (xs :: [*]) where
  type StatesHandlerRes hf xs :: [*]
  getStatesHandler :: Proxy hf -> V xs -> m (V (StatesHandlerRes hf xs))

instance StatesHandlerProv m hf '[] where
  type StatesHandlerRes hf '[] = '[]
  getStatesHandler _ _ = SafeUndef.undefined

instance 
  ( StateHandlerProv m hf x
  , StatesHandlerProv m hf xs
  , newRes ~ Eval (hf x)
  , res ~ Union newRes (StatesHandlerRes hf xs)
  , Liftable newRes res
  , Liftable (StatesHandlerRes hf xs) res
  , Monad m
  ) => StatesHandlerProv m hf (x ': xs) where
  type StatesHandlerRes hf (x ': xs) = Union (Eval (hf x)) (StatesHandlerRes hf xs)
  getStatesHandler px_hf v_x_xs = case popVariantHead v_x_xs of
    Right x -> liftVariant <$> getHandler px_hf x
    Left v_xs -> liftVariant <$> getStatesHandler px_hf v_xs

newtype CResPar (m :: * -> *) (hf :: * -> Exp [*])  (initRes :: *) = CResPar (STransApp IdentityT m NoSplitter '[()] '[initRes] '[] ())
newtype CRes (hf :: * -> Exp [*]) (res :: *)  = CRes res 

compRes :: forall hf m res. STransApp IdentityT m NoSplitter '[()] '[res] '[] () -> CResPar m hf res
compRes = CResPar

instance 
  ( Monad m
  ) => MkRes m (CResPar m hf res)  where
  type ResSt m (CResPar m hf res ) = CRes hf res  
  mkRes (CResPar (MkApp (STrans t))) = 
    fmap (getRes (Proxy @hf)) (runIdentityT $ t NoSplitter (variantFromValue ()))   

extractRes :: Either (V ex) (V rs, a) -> V rs
extractRes (Right (v,_)) = v

getRes :: Proxy hf -> Either (V '[]) (V '[x],()) -> CRes hf x 
getRes _px ei = CRes (variantToValue (extractRes ei))  

hfRes :: Proxy hf -> res -> CRes hf res
hfRes _px res = CRes res

--applyHnd :: (Monad m, _) => V xs -> (V xs -> V rs) -> m (V (First (Eval (f NoSplitter xs))))
--applyHnd v_xs (MkHnd (STrans t)) = fmap extractRes (runIdentityT $ t NoSplitter v_xs) 

--

type StCs hf res name = St (CRes hf res) name  
type instance StateTrans (StCs hf res name) = StateTrans res

instance 
  ( ns ~ NextStates (ExtendedStateTrans res) 
  , StatesHandlerProv m hf ns
  , StWrapVar (CRes hf) (StatesHandlerRes hf ns) name
  , Transition m (ExtendedStateTrans res)
  , ExtendStateTrans res
  , MonadIO m
  , StateTrans res ~ 'Dynamic
  ) => Transition m (StCs hf res name) where
  type NextStates (StCs hf res name) = StWrapList (CRes hf) name (StatesHandlerRes hf (NextStates  (ExtendedStateTrans res)))
  next (St (CRes res)) cb = 
    next (extendStateTrans res) (\v_next ->  
      do
        v_ys <- getStatesHandler (Proxy @hf) v_next  
        cb (asStWrapVar (hfRes (Proxy @hf)) (Named @name) v_ys))

newtype CompReq m res xs = CompReq (STransApp IdentityT m NoSplitter '[res] xs '[] ())
instance Show (CompReq m res xs) where
  show _ = "CompReq"

compReq :: Monad m => STransApp IdentityT m NoSplitter '[res] xs '[] () -> CompReq m res xs
compReq = CompReq

applyHnd' :: (Monad m) => res -> CompReq m res xs -> m (V xs)
applyHnd' res (CompReq (MkApp (STrans t))) = fmap extractRes (runIdentityT $ t NoSplitter (variantFromValue res)) 

instance 
  ( Monad m 
  , StWrapVar (CRes hf) xs name
  ) => Request m (CompReq m res xs) (StCs hf res name) where
  type ReqResult (CompReq m res xs) (StCs hf res name) = StWrapList (CRes hf) name xs 
  request cmpReq (St (CRes res)) = do 
    v_ys <- applyHnd' res cmpReq
    return (asStWrapVar (hfRes (Proxy @hf)) (Named @name) v_ys)

instance  
  ( Monad m
  , TermState m res
  ) => TermState m (StCs hf res name) where
    terminate (St (CRes res)) = terminate res 

invokeC :: (_) => Named name -> m req -> STrans q m sp _ _ _ _ ()
invokeC named m_req = bindT (op m_req) (invoke named)  

type instance UnwrapContent (StCs hf res name) = res
