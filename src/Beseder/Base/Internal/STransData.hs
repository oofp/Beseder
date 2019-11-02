{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.STransData where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Control.Monad.Cont (ContT)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransMonad

data STransData (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*]) (sfunc :: * -> [*] -> ([*],[*]) -> *) (a :: *) where 
  Return :: a -> STransData m sp xs xs '[] (ReturnFunc a) a
  Bind :: 
    ( '(rs1,ex1) ~ Eval (f1 sp xs)
    , '(rs2,ex2) ~ Eval (f2 sp rs1)  
    , Concat ex1 ex2 ~ ex
    --, KnownNat (Length ex1)
    ) => STransData m sp xs rs1 ex1 f1 a -> (a -> STransData m sp rs1 rs2 ex2 f2 b) -> STransData m sp xs rs2 ex (BindFunc f1 f2) b 
  Compose ::
    ( '(rs1,ex1) ~ Eval (f1 sp xs)
    , '(rs2,ex2) ~ Eval (f2 sp rs1)  
    , Concat ex1 ex2 ~ ex
    --, KnownNat (Length ex1)
    ) => STransData m sp xs rs1 ex1 f1 () -> STransData m sp rs1 rs2 ex2 f2 b -> STransData m sp xs rs2 ex (ComposeFunc f1 f2) b 
  NewRes ::
    ( res ~ St (ResSt m resPars) name
    , zs ~ AppendToTupleList xs res
    , '(rs,ex) ~ ListSplitterRes2 sp zs
    ) =>  Named name -> resPars -> STransData m sp xs rs ex (NewResFunc resPars name m) ()
  Invoke ::
    ( zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
    , '(rs,ex) ~ ListSplitterRes2 sp zs
    ) => Named name -> req -> STransData m sp xs rs ex (InvokeAllFunc req name) ()
  Clear ::
    ( zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
    , '(rs,ex) ~ ListSplitterRes2 sp zs
    ) => Named name -> STransData m sp xs rs ex (ClearAllFunc name) ()
  NextEv' ::
    ( zs ~ NextStates (TransWrap xs)
    , '(rs,ex) ~ ListSplitterRes2 sp zs
    ) => STransData m sp xs rs ex GetNextAllFunc ()
  ClearResources' :: STransData m sp xs '[()] '[] ClearAllVarFunc ()
  Try :: forall sp1 sp xs xs_sub ex_sub rs_sub ex ex1 m f_sub rs. 
    ( '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
    , '(rs_sub,ex) ~ Eval (f_sub (sp :&& sp1) xs_sub)
    , '(rs, ex1) ~ ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 sp1 xs) f_sub (sp :&& sp1) xs))
    ) => STransData m (sp :&& sp1) xs_sub rs_sub ex f_sub () -> STransData m sp xs rs ex1 (EmbedFunc sp1 f_sub) ()
  On :: forall sp1 sp xs xs_sub ex_sub rs_sub ex m f_sub rs1. 
    ( '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
    , rs1 ~ Union rs_sub ex_sub  
    , '(rs_sub,ex) ~ Eval (f_sub sp xs_sub)
    ) => STransData m sp xs_sub rs_sub ex f_sub () -> STransData m sp xs rs1 ex (CaptureFunc sp1 f_sub) ()
  OpRes :: Named name -> (x -> m a) -> STransData m sp xs xs ('[]) (OpResFunc name x) a
  NextSteps :: -- forall n sp xs rs ex m.
    ( '(rs,ex) ~ Eval (NextStepsFunc n sp xs)
    ) => Proxy n -> STransData m sp xs rs ex (NextStepsFunc n) ()

(>:>) ::
  ( '(rs1,ex1) ~ Eval (f1 sp xs)
  , '(rs2,ex2) ~ Eval (f2 sp rs1)  
  , Concat ex1 ex2 ~ ex
  --, KnownNat (Length ex1)
  ) => STransData m sp xs rs1 ex1 f1 () -> STransData m sp rs1 rs2 ex2 f2 b -> STransData m sp xs rs2 ex (ComposeFunc f1 f2) b 
(>:>) = Compose
infixr 1 >:>

(>*>) :: 
  ( '(rs1,ex1) ~ Eval (f1 sp xs)
  , '(rs2,ex2) ~ Eval (f2 sp rs1)  
  , Concat ex1 ex2 ~ ex
  ) => STransData m sp xs rs1 ex1 f1 a -> (a -> STransData m sp rs1 rs2 ex2 f2 b) -> STransData m sp xs rs2 ex (BindFunc f1 f2) b 
(>*>) = Bind
infixr 1 >*>

instance STransMonad (STransData m) where
  type Cx (STransData m) sp xs rs ex f = (Eval (f sp xs) ~ '(rs,ex))
  st_return = Return   
  st_compose = Compose
  st_bind = Bind
