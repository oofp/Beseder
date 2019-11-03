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
{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.Base.Internal.STransDataDo where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Utils.ListHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransData

newRes ::
  ( res ~ St (ResSt m resPars) name
  , zs ~ AppendToTupleList xs res
  , '(rs,ex) ~ ListSplitterRes2 sp zs
  ) =>  Named name -> resPars -> STransData m sp xs rs ex (NewResFunc resPars name m) ()
newRes = NewRes

invoke ::
  ( zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  , '(rs,ex) ~ ListSplitterRes2 sp zs
  ) => Named name -> req -> STransData m sp xs rs ex (InvokeAllFunc req name) ()
invoke = Invoke
  
clear ::
  ( zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , '(rs,ex) ~ ListSplitterRes2 sp zs
  ) => Named name -> STransData m sp xs rs ex (ClearAllFunc name) ()
clear = Clear

nextEv' ::
  ( zs ~ NextStates (TransWrap xs)
  , '(rs,ex) ~ ListSplitterRes2 sp zs
  ) => STransData m sp xs rs ex GetNextAllFunc ()
nextEv' = NextEv'

termAndClearAllResources :: STransData m sp xs '[()] '[] ClearAllVarFunc ()
termAndClearAllResources = ClearResources'

try :: forall sp1 sp xs xs_sub ex_sub rs_sub ex ex1 m f_sub rs. 
  ( '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
  , '(rs_sub,ex) ~ Eval (f_sub (sp :&& sp1) xs_sub)
  , '(rs, ex1) ~ ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 sp1 xs) f_sub (sp :&& sp1) xs))
  ) => STransData m (sp :&& sp1) xs_sub rs_sub ex f_sub () -> STransData m sp xs rs ex1 (EmbedFunc sp1 f_sub) ()
try = Try

on :: forall sp1 sp xs xs_sub ex_sub rs_sub ex m f_sub rs1. 
  ( '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
  , rs1 ~ Union rs_sub ex_sub  
  , '(rs_sub,ex) ~ Eval (f_sub sp xs_sub)
  ) => STransData m sp xs_sub rs_sub ex f_sub () -> STransData m sp xs rs1 ex (CaptureFunc sp1 f_sub) ()
on = On

opRes :: Named name -> (x -> m a) -> STransData m sp xs xs ('[]) (OpResFunc name x) a
opRes = OpRes

nextEv :: STransData m sp xs _ _ _ ()
nextEv = On @Dynamics nextEv'


skipAll :: -- forall sp xs xs_sub ex_sub rs_sub ex ex1 m f_sub rs n.   
  ( '(xs_sub, ex_sub) ~ ListSplitterRes2 Dynamics xs
  , n ~ TotalSteps (sp :&& Dynamics) xs GetNextAllFunc
  , f_sub ~ NextStepsFunc n
  , '(rs_sub,ex) ~ Eval (f_sub (sp :&& Dynamics) xs_sub)
  , '(rs, ex1) ~ ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 Dynamics xs) f_sub (sp :&& Dynamics) xs)) 
  ) => STransData m sp xs rs ex1 (EmbedFunc Dynamics f_sub) ()
skipAll = 
  let px_n :: Proxy n
      px_n = Proxy
  in Try @Dynamics (NextSteps px_n)

skipTo' :: forall sp1 sp xs xs_sub ex_sub rs_sub ex ex1 m f_sub rs n.   
  ( '(xs_sub, ex_sub) ~ ListSplitterRes2 (Not sp1) xs
  , n ~ TotalSteps (sp :&& (Not sp1)) xs GetNextAllFunc
  , f_sub ~ NextStepsFunc n
  , '(rs_sub,ex) ~ Eval (f_sub (sp :&& (Not sp1)) xs_sub)
  , '(rs, ex1) ~ ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 (Not sp1) xs) f_sub (sp :&& (Not sp1)) xs)) 
  ) => sp1 -> STransData m sp xs rs ex1 (EmbedFunc (Not sp1) f_sub) ()
skipTo' _sp1 = 
  let px_n :: Proxy (TotalSteps (sp :&& (Not sp1)) xs GetNextAllFunc)
      px_n = Proxy
  in Try @(Not sp1) (NextSteps px_n)

skipTo :: forall sp1 sp xs xs_sub ex_sub rs_sub ex ex1 m f_sub rs n.   
  ( '(xs_sub, ex_sub) ~ ListSplitterRes2 (Not sp1) xs
  , n ~ TotalSteps (sp :&& (Not sp1)) xs GetNextAllFunc
  , f_sub ~ NextStepsFunc n
  , '(rs_sub,ex) ~ Eval (f_sub (sp :&& (Not sp1)) xs_sub)
  , '(rs, ex1) ~ ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 (Not sp1) xs) f_sub (sp :&& (Not sp1)) xs)) 
  , GetInstance sp1
  ) => STransData m sp xs rs ex1 (EmbedFunc (Not sp1) f_sub) ()
skipTo  = 
  let sp1 :: sp1
      sp1 = getInstance
  in skipTo' sp1

liftIO :: IO a -> STransData m sp xs xs ('[]) LiftIOFunc a
liftIO = LiftIO

whatNext :: STransData m sp xs xs ('[]) WhatNextFunc (Proxy xs)
whatNext = WhatNext

noop :: STransData m sp xs xs ('[]) NoopFunc ()
noop = Noop

op :: m a -> STransData m sp xs xs ('[]) OpFunc a
op = Op

forever :: 
  ('(xs,ex) ~ Eval (f sp xs)
  ) => STransData m sp xs xs ex f () -> STransData m sp xs ('[]) ex (ForeverFunc f) ()
forever = Forever