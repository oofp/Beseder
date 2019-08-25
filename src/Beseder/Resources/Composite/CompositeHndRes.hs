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
{-# LANGUAGE InstanceSigs #-}

module  Beseder.Resources.Composite.CompositeHndRes 
  ( StCrH
  , CrReqH (..)
  , CrReqF (..)
  , CrResH (..)
  , Handler (..)
  ) where

import            Protolude hiding (First)
import            GHC.TypeLits (Symbol, ErrorMessage (..), TypeError)
import            Control.Concurrent.STM.TVar
import            Haskus.Utils.Variant 
import            Haskus.Utils.Tuple
import            Haskus.Utils.Types.List
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


type Handler funcData dict m xs = ToTrans funcData dict IdentityT m NoSplitter xs ()

{-
type family CanHandleList (hfunc :: * -> [*] -> ([*],[*]) -> *) dict (m :: * -> *) (xs :: [[*]]) :: Constraint where
  CanHandleList hfunc dict m '[] = ()
  CanHandleList hfunc m dict (x ': xs) = (Handler hfunc dict m x, CanHandleList hfunc dict m xs)  
-}

data CrH a hfunc dict = CrH a dict

type StCrH a hfunc dict name = St (CrH a hfunc dict) name

type instance StateTrans (StCrH a hfunc dict name) = StateTrans a

data CrResH m initState sfunc hfunc dict = 
  CrResH 
    (STrans IdentityT m NoSplitter '[()] '(('[initState]),'[]) sfunc ()) 
    dict

instance Show (CrResH m initState sfunc xs hfunc) where
  show _ = "CreateHComposite"

instance 
    ( Monad m
    ) => MkRes m (CrResH m initState sfunc hfunc dict)  where
    type ResSt m (CrResH m initState sfunc hfunc dict) = CrH initState hfunc dict
    mkRes (CrResH t dict) = 
      fmap (\a -> CrH (getRes a) dict) (runIdentityT $ applyTrans t NoSplitter (return (variantFromValue ())))  
  
  
getRes :: Either (V '[]) (V '[x],()) -> x
getRes (Right (v_x, ())) = variantToValue v_x  

getResults :: Either (V '[]) (V rs,()) ->  V rs
getResults (Right (v_rs, ())) = v_rs  

data CrReqH m x sfunc = CrReqH (STrans IdentityT m NoSplitter '[x] '(First (Eval (sfunc NoSplitter '[x])),'[]) sfunc ())
instance Show (CrReqH m x sfunc) where
  show _ = "InvokeHComposite"

data CrReqF m sfunc = CrReqF 
instance Show (CrReqF m sfunc) where
  show _ = "InvokeHCompositeF"
  
type family StCrHList (rs :: [*]) hfunc dict (name :: Symbol) where
    StCrHList '[] hfunc dict name = '[]
    StCrHList (x ': xs) hfunc dict name = St (CrH x hfunc dict) name ': StCrHList xs hfunc dict name

class MkStCrHVar (name :: Symbol) hfunc dict (xs :: [*]) where
  mkStCrHVar ::  Named name -> Proxy hfunc -> dict -> V xs -> V (StCrHList xs hfunc dict name)  

instance  MkStCrHVar name hfunc dict '[] where
  mkStCrHVar _ _ _ _ = undefined

stCrHWithName :: Named name -> Proxy hfunc -> x -> dict -> StCrH x hfunc dict name
stCrHWithName named px x dict = St (CrH x dict)

instance  
  ( MkStCrHVar name hfunc dict xs 
  , Liftable '[St (CrH x hfunc dict) name] (StCrHList (x ': xs) hfunc dict name)
  , Liftable (StCrHList xs hfunc dict name) (StCrHList (x ': xs) hfunc dict name)
  ) => MkStCrHVar name hfunc dict (x ': xs) where
  mkStCrHVar named px dict v = case popVariantHead v of
    Right x -> liftVariant (variantFromValue (stCrHWithName named px x dict))
    Left v_xs -> liftVariant (mkStCrHVar named px dict v_xs) 
  
instance 
  ( Monad m
  , xs ~ (First (Eval (sfunc NoSplitter '[x])))
  , MkStCrHVar name hfunc dict xs
  ) => Request m (CrReqH m x sfunc) (StCrH x hfunc dict name) where
  type ReqResult (CrReqH m x sfunc) (StCrH x hfunc dict name) = StCrHList (First (Eval (sfunc NoSplitter '[x]))) hfunc dict name 
  request (CrReqH t) (St (CrH x dict))=
    let named :: Named name
        named = Named  
        px :: Proxy hfunc
        px = Proxy
    in fmap (mkStCrHVar named px dict . getResults) (runIdentityT $ applyTrans t NoSplitter (return (variantFromValue x)))  

instance 
  ( Monad m
  , MkStCrHVar name hfunc dict xs
  , Handler sfunc dict m '[x]
  , Eval (sfunc NoSplitter '[x]) ~ '(xs, '[])
  ) => Request m (CrReqF m sfunc) (StCrH x hfunc dict name) where
  type ReqResult (CrReqF m sfunc) (StCrH x hfunc dict name) = StCrHList (First (Eval (sfunc NoSplitter '[x]))) hfunc dict name 
  request CrReqF (St (CrH x dict))=
    let named :: Named name
        named = Named  
        px :: Proxy hfunc
        px = Proxy
        ps :: Proxy sfunc
        ps = Proxy
        t = reifyTrans ps dict
    in fmap (mkStCrHVar named px dict . getResults) (runIdentityT $ applyTrans t NoSplitter (return (variantFromValue x)))  

instance  
  ( MonadIO m
  , ExtendStateTrans x
  , StateTrans x ~ 'Dynamic
  , Transition m (ExtendedStateTrans x)
  , Handler hfunc dict m (NextStates (ExtendedStateTrans x))
  , Eval (hfunc NoSplitter (NextStates (ExtendedStateTrans x))) ~ '(rs, '[])
  , MkStCrHVar name hfunc dict rs
  ) => Transition m  (StCrH x hfunc dict name) where
  type NextStates (StCrH x hfunc dict name) = StCrHList (First (Eval (hfunc NoSplitter (NextStates (ExtendedStateTrans x))))) hfunc dict name 
  next (St (CrH x dict)) cb = 
    let named :: Named name
        named = Named 
        px :: Proxy hfunc
        px = Proxy 
    in next (extendStateTrans x) 
      (\v_next -> do 
        let t = reifyTrans px dict
        v_res <- fmap (mkStCrHVar named px dict . getResults) (runIdentityT $ applyTrans t NoSplitter (return v_next))   
        cb v_res)

instance  
  ( Monad m
  , TermState m x
  ) => TermState m  (StCrH x hfunc dict name) where
    terminate (St (CrH x hnd )) = terminate x
  