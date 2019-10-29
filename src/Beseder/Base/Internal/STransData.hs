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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.STransData where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.STransIx
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef

data STransData (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*]) (sfunc :: * -> [*] -> ([*],[*]) -> *) (a :: *) where 
  Return :: a -> STransData m sp xs xs '[] (ReturnFunc a) a
  Bind :: 
    ( '(rs1,ex1) ~ Eval (f1 sp xs)
    , '(rs2,ex2) ~ Eval (f2 sp rs1)  
    , Concat ex1 ex2 ~ ex
    ) => STransData m sp xs rs1 ex1 f1 a -> (a -> STransData m sp rs1 rs2 ex2 f2 b) -> STransData m sp xs rs2 ex (BindFunc f1 f2) b 
  Compose ::
    ( '(rs1,ex1) ~ Eval (f1 sp xs)
    , '(rs2,ex2) ~ Eval (f2 sp rs1)  
    , Concat ex1 ex2 ~ ex
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
    (zs ~ NextStates (TransWrap xs)
    , '(rs,ex) ~ ListSplitterRes2 sp zs
    ) => STransData m sp xs rs ex GetNextAllFunc ()
  ClearResources' :: STransData m sp xs '[()] '[] ClearAllVarFunc ()

(>:>) ::
  ( '(rs1,ex1) ~ Eval (f1 sp xs)
  , '(rs2,ex2) ~ Eval (f2 sp rs1)  
  , Concat ex1 ex2 ~ ex
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

type family STransCon (sfunc :: * -> [*] -> ([*],[*]) -> *) :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp (Constraint)   

type instance STransCon (ReturnFunc a) = ReturnCon 
data ReturnCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (ReturnCon q m sp xs rs ex) = ()

type instance STransCon (NewResFunc resPars name m) = NewResCon name resPars
data NewResCon :: Symbol -> * -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (NewResCon name resPars q m sp xs rs ex) = NewResConFam name resPars (St (ResSt m resPars) name) m sp xs rs ex
type family NewResConFam name resPars res m sp xs rs ex where
  NewResConFam name resPars res m sp xs rs ex = NewResConFam' name resPars res m sp xs rs ex (AppendToTupleList xs res) 
type family NewResConFam' name resPars res m sp xs rs ex zs where
  NewResConFam' name resPars res m sp xs rs ex zs = 
    ( MkRes m resPars
    , SplicC sp rs ex zs
    , KnownSymbol name
    , AppendToTuple (Variant xs) res
    , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
    , IsTypeUniqueList name xs 
    ) 

type instance STransCon (InvokeAllFunc req name) = InvokeCon req name
data InvokeCon :: * -> Symbol -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (InvokeCon req name q m sp xs rs ex) = InvokeConFam req name m sp xs rs ex (ReqResult (NamedRequest req name) (VWrap xs NamedTuple))
type family InvokeConFam req name m sp xs rs ex zs where
  InvokeConFam req name m sp xs rs ex zs =
    ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
    , Show req
    , KnownSymbol name
    , SplicC sp rs ex zs
    )

type instance STransCon (ClearAllFunc name) = ClearCon name
data ClearCon :: Symbol -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (ClearCon name q m sp xs rs ex) = ClearConFam name m sp xs rs ex (ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr))
type family ClearConFam name m sp xs rs ex zs where
  ClearConFam name m sp xs rs ex zs =
    ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
    , KnownSymbol name
    , SplicC sp rs ex zs
    )

type instance STransCon (ComposeFunc f1 f2) = ComposeCon f1 f2
data ComposeCon :: (* -> [*] -> ([*],[*]) -> *) -> (* -> [*] -> ([*],[*]) -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (ComposeCon f1 f2 q m sp xs rs ex) = ComposeConFam f1 f2 q m sp xs rs ex (Eval (f1 sp xs))
type family ComposeConFam f1 f2 q m sp xs rs ex rs1_ex1 where
  ComposeConFam f1 f2 q m sp xs rs ex '(rs1,ex1) = ComposeConFam' f1 f2 q m sp xs rs ex rs1 ex1 (Eval (f2 sp rs1))
type family ComposeConFam' f1 f2 q m sp xs rs ex rs1 ex1 rs2_ex2 where
  ComposeConFam' f1 f2 q m sp xs rs ex rs1 ex1 '(rs2,ex2) = 
    ( Eval (STransCon f1 q m sp xs rs1 ex1) 
    , Eval (STransCon f2 q m sp rs1 rs2 ex2) 
    , rs ~ rs2
    , KnownNat (Length ex1)
    )

type instance STransCon (BindFunc f1 f2) = BindCon f1 f2
data BindCon :: (* -> [*] -> ([*],[*]) -> *) -> (* -> [*] -> ([*],[*]) -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (BindCon f1 f2 q m sp xs rs ex) = ComposeConFam f1 f2 q m sp xs rs ex (Eval (f1 sp xs))

type instance STransCon GetNextAllFunc = NextEvCon 
data NextEvCon ::  ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (NextEvCon q m sp xs rs ex) = NextEvConFam q m sp xs rs ex (NextStates (TransWrap xs))
type family NextEvConFam q m sp xs rs ex zs where
  NextEvConFam q m sp xs rs ex zs =
    ( Transition m (TransWrap xs) 
    , SplicC sp rs ex zs
    , q ~ (ContT Bool)
    )


type instance STransCon ClearAllVarFunc = ClearAllVarCon
data ClearAllVarCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint
type instance Eval (ClearAllVarCon q m sp xs rs ex) = TermState m (ClrVar xs)

--
interpret :: 
  ( MonadTrans q
  , Monad (q m)
  , Eval ((STransCon f) q m sp xs rs ex)
  ) => STransData m sp xs rs ex f a -> STrans q m sp xs rs ex f a
interpret (Return a) = returnT a
interpret (NewRes named resPars) = newRes named resPars
interpret (Invoke named req) = invoke named req
interpret (Clear named) = clear named
interpret (Compose sd1 sd2) = 
  composeT (interpret sd1) (interpret sd2)
interpret (Bind sd1 func) = 
  bindT (interpret sd1) (interpret . func)
interpret NextEv' = nextEv' 
interpret ClearResources' = termAndClearAllResources 
  

