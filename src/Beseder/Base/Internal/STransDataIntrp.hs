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

module Beseder.Base.Internal.STransDataIntrp 
  ( interpret 
  ) where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.STransIx
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransData 
import           Beseder.Utils.VariantHelper

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

type instance STransCon (EmbedFunc sp1 f_sub) = EmbedCon sp1 f_sub
data EmbedCon :: * -> (* -> [*] -> ([*],[*]) -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (EmbedCon sp1 f_sub q m sp xs rs ex1) = EmbedConFam sp1 f_sub q m sp xs rs ex1 (ListSplitterRes2 sp1 xs)
type family EmbedConFam sp1 f_sub q m sp xs rs ex1 xs_sub_ex_sub where
  EmbedConFam sp1 f_sub q m sp xs rs ex1 '(xs_sub, ex_sub) = EmbedConFam' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub (Eval (f_sub (sp :&& sp1) xs_sub))
type family EmbedConFam' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub_ex where
    EmbedConFam' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub '(rs_sub,ex) = EmbedConFam'' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex (Union rs_sub (Union ex_sub ex))
type family EmbedConFam'' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex zs where
  EmbedConFam'' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex zs = EmbedConFam''' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex zs (ListSplitterRes2 sp zs)
type family EmbedConFam''' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex zs rs_ex1 where
  EmbedConFam''' sp1 f_sub q m sp xs rs ex1 xs_sub ex_sub rs_sub ex zs rs_ex1 = 
    ( rs_ex1 ~ '(rs,ex1) 
    , SplicC sp1 xs_sub ex_sub xs
    , Liftable ex zs
    , Liftable ex_sub zs
    , Liftable rs_sub zs
    , SplicC sp rs ex1 zs
    , GetInstance sp
    , GetInstance sp1
    , Eval (STransCon f_sub q m (sp :&& sp1) xs_sub rs_sub ex) 
    )

type instance STransCon (CaptureFunc sp1 f_sub) = CaptureCon sp1 f_sub
data CaptureCon :: * -> (* -> [*] -> ([*],[*]) -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (CaptureCon sp1 f_sub q m sp xs rs1 ex1) = CaptureConFam sp1 f_sub q m sp xs rs1 ex1 (ListSplitterRes2 sp1 xs)
type family CaptureConFam sp1 f_sub q m sp xs rs ex1 xs_sub_ex_sub where
  CaptureConFam sp1 f_sub q m sp xs rs1 ex1 '(xs_sub, ex_sub) = CaptureConFam' sp1 f_sub q m sp xs rs1 ex1 xs_sub ex_sub (Eval (f_sub sp xs_sub))
type family CaptureConFam' sp1 f_sub q m sp xs rs1 ex1 xs_sub ex_sub rs_sub_ex where
  CaptureConFam' sp1 f_sub q m sp xs rs1 ex1 xs_sub ex_sub '(rs_sub,ex) = 
    ( GetInstance sp1
    , VariantSplitter xs_sub ex_sub xs
    , rs1 ~ Union rs_sub ex_sub  
    , Liftable rs_sub rs1
    , Liftable ex_sub rs1
    , ListSplitter sp1 xs
    , Eval (STransCon f_sub q m sp xs_sub rs_sub ex) 
    )

type instance STransCon (OpResFunc name x)  = OpResCon name x
data OpResCon :: Symbol -> * -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (OpResCon name x q m sp xs rs1 ex1) = GetTypeByNameVar name x xs
    
type instance STransCon (NextStepsFunc n) = NextStepsCon n
data NextStepsCon :: NatOne -> ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (NextStepsCon steps q m sp xs rs ex) = 
  ( q ~ ContT Bool
  , NextSteps steps m sp xs rs ex (StepsFuncFam steps GetNextAllFunc)
  )

--
interpret :: 
  ( MonadTrans q
  , Monad (q m)
  , Monad m
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
interpret (Try sd) = embed getInstance (interpret sd) 
interpret (On sd) = capture getInstance (interpret sd) 
interpret (OpRes named getter) = opRes named getter 
interpret (NextSteps px) = nextSteps' px 
