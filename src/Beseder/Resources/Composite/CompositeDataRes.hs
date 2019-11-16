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

module  Beseder.Resources.Composite.CompositeDataRes
  ( CdRes (..)
  , CRdPar (..)
  , StCd
  , CompositeDataRes (..)
  , CompositeDataReq (..)
  ) where

import            Protolude hiding (First)
import            GHC.TypeLits (Symbol, ErrorMessage (..), TypeError)
import            Haskus.Utils.Variant 
import            Haskus.Utils.Types.List
import            Beseder.Base.Base
import            Beseder.Base.Common
import            Beseder.Utils.ListHelper
import            Beseder.Base.Internal.SplitOps
import            Beseder.Base.Internal.Core
import            Beseder.Base.Internal.StHelper
import            Beseder.Base.Internal.STransIx
import            Beseder.Base.Internal.STransData
import            Beseder.Base.Internal.STransDataIntrp
import qualified  Beseder.Base.Internal.STransIxDo as SDo 
import            Beseder.Base.Internal.TypeExp
import            Control.Monad.Identity (IdentityT, runIdentityT)
import            qualified GHC.Show (Show (..))

class CompositeDataRes m rt where
  type CreateFunc m rt :: * -> [*] -> Exp ([*], [*]) 
  type HandlerFunc m rt :: * -> [*] -> Exp ([*], [*]) 
  resInit :: rt -> STransData m NoSplitter (CreateFunc m rt) ()
  stateHandler ::  Proxy rt -> STransData m NoSplitter (HandlerFunc m rt) ()

class CompositeDataReq m req where
  type RequestFunc m req :: * -> [*] -> Exp ([*], [*]) 
  type ReqSplitter req 
  reqHandler :: req -> STransData m NoSplitter (RequestFunc m req) ()
  
newtype CRdPar rt = CRdPar rt   
newtype CdRes m rt (resData :: *)  = CdRes resData 
type StCd m rt resData name = St (CdRes m rt resData) name  
type instance StateTrans (StCd m rt resData name) = StateTrans resData

instance 
  ( Monad m
  , CompositeDataRes m res
  , initState ~ FromSingletonList (First (Eval ((CreateFunc m res) NoSplitter '[()])))
  , Interpretable IdentityT m NoSplitter '[()] '[initState] '[] (CreateFunc m res) 
  ) => MkRes m (CRdPar res)  where
  type ResSt m (CRdPar res) = CdRes m res (FromSingletonList (First (Eval ((CreateFunc m res) NoSplitter '[()]))))
  mkRes (CRdPar res) =  
    let strans :: STrans IdentityT m NoSplitter '[()] '[initState] '[] (CreateFunc m res) ()
        strans = interpret (resInit res)
    in
      fmap (getRes (Proxy @m) (Proxy @res)) (runIdentityT $ runTrans strans NoSplitter (variantFromValue ()))      
       
extractRes :: Either (V ex) (V rs, a) -> V rs
extractRes (Right (v,_)) = v

getRes :: Proxy m -> Proxy res -> Either (V '[]) (V '[x],()) -> CdRes m res x 
getRes _pxm _px ei = CdRes (variantToValue (extractRes ei))  
      
instance 
  ( CompositeDataRes m rt
  , MonadIO m
  , StateTrans resData ~ 'Dynamic
  , ns ~ NextStates (ExtendedStateTrans resData) 
  , hfunc ~ HandlerFunc m rt
  , hndRes ~ First (Eval (hfunc NoSplitter ns))
  , Interpretable (IdentityT) m NoSplitter ns hndRes '[] hfunc
  , StWrapVar (CdRes m rt) hndRes name
  , Transition m (ExtendedStateTrans resData)
  , ExtendStateTrans resData
  ) => Transition m (StCd m rt resData name) where
  type NextStates (StCd m rt resData name) = StWrapList (CdRes m rt) name (First (Eval ((HandlerFunc m rt) NoSplitter (NextStates (ExtendedStateTrans resData)))))
  next (St (CdRes resData)) cb =  
    next (extendStateTrans resData) (\v_next ->  
      do
        let strans :: STrans (IdentityT) m NoSplitter ns hndRes '[] hfunc ()
            strans =  interpret (stateHandler (Proxy @rt))
        ei_v_ys_none <- runIdentityT (runTrans strans NoSplitter v_next)    
        let Right (v_ys,()) = ei_v_ys_none
        cb (asStWrapVar (cdRes (Proxy @m) (Proxy @rt)) (Named @name) v_ys))

cdRes :: Proxy m -> Proxy rt -> resData -> CdRes m rt resData
cdRes _pm _px resData = CdRes resData

instance 
  ( CompositeDataReq m req
  , reqfunc ~ RequestFunc m req
  , Interpretable (IdentityT) m NoSplitter '[res] reqRes '[] reqfunc
  , reqRes ~ First (Eval (reqfunc NoSplitter '[res]))
  , StWrapVar (CdRes m rt) reqRes name
  , '[res] ~ ListSplitterRes (ReqSplitter req) '[res]      
  , MonadIO m
  ) => Request m req (StCd m rt res name) where
  type ReqResult req (StCd m rt res name) = StWrapList (CdRes m rt) name (First (Eval ((RequestFunc m req) NoSplitter '[res])))
  request req (St (CdRes res)) = do
    let strans :: STrans (IdentityT) m NoSplitter '[res] reqRes '[] reqfunc ()
        strans = interpret (reqHandler req)
    ei_v_ys_none <- runIdentityT (runTrans strans NoSplitter (variantFromValue res))    
    let Right (v_ys,()) = ei_v_ys_none
    return $ asStWrapVar (cdRes (Proxy @m) (Proxy @rt)) (Named @name) v_ys

instance  
  ( Monad m
  , TermState m res
  ) => TermState m (StCd m rt res name) where
    terminate (St (CdRes res)) = terminate res 
  
  