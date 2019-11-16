{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.STransDef where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           GHC.TypeLits
import           Haskus.Utils.Types.List
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Utils.ListHelper
import           Beseder.Base.Internal.NatOne
import           Haskus.Utils.Variant

data ReturnFunc :: res -> * -> [*] -> Exp ([*],[*])
type instance Eval (ReturnFunc res sp xs) = '(xs, '[])

data OpResFunc :: name -> * -> * -> [*] -> Exp ([*],[*])
type instance Eval (OpResFunc name x sp xs) = '(xs, '[])

data GetFunc :: name -> * -> * -> [*] -> Exp ([*],[*])
type instance Eval (GetFunc name x sp xs) = '(xs, '[])

data OpFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (OpFunc sp xs) = '(xs, '[])

-- transformation defunc data and their evaluators
data WithResFunc :: res -> * -> [*] -> Exp ([*],[*])
type instance Eval (WithResFunc res sp (x ': ys)) = ListSplitterRes2 sp (Union '[AppendToTupleResult x res] ys)

data OrderFunc :: [Symbol] -> * -> [*] -> Exp ([*],[*])
type instance Eval (OrderFunc names sp xs) = ListSplitterRes2 sp (ListOfVar (OrderByNameRes names (V xs)))

data WithResAllFunc :: res -> * -> [*] -> Exp ([*],[*])
type instance Eval (WithResAllFunc res sp xs) = ListSplitterRes2 sp (AppendToTupleList xs res)

data NewResFunc :: (resPars :: *) -> name -> (* -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (NewResFunc resPars name m sp xs) = ListSplitterRes2 sp (AppendToTupleList xs (St (ResSt m resPars) name))

data InvokeAllFunc :: (req :: *) -> name -> * -> [*] -> Exp ([*],[*])
type instance Eval (InvokeAllFunc req name sp xs) = ListSplitterRes2 sp (ReqResult (NamedRequest req name) (VWrap xs NamedTuple))

data ClearAllFunc :: Symbol -> * -> [*] -> Exp ([*],[*])
type instance Eval (ClearAllFunc name sp xs) = ListSplitterRes2 sp (ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr))

data GetNextFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (GetNextFunc sp (x ': ys)) = ListSplitterRes2 sp (Union (NextStates (ExtendedStateTrans x)) ys)

data GetNextAllFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (GetNextAllFunc sp xs) = ListSplitterRes2 sp (NextStates (TransWrap xs))

data InvokeFunc :: (req :: *) -> name -> * -> [*] -> Exp ([*],[*])
type instance Eval (InvokeFunc req name sp (x ': ys)) = ListSplitterRes2 sp (Union (ReqResult req (TargetByName name x)) ys)

data ClearAllVarFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (ClearAllVarFunc sp xs) = '(('[()]) , '[])

type family ComposeFam (as_ex :: ([*],[*])) (sp :: *) (f_b :: * -> [*] -> ([*],[*]) -> *) :: ([*],[*]) where
  ComposeFam '(as1,ex1) sp f_b = ConcatExs (Eval (f_b sp as1)) ex1

type family UnionExs (bs_ex :: ([*],[*]))  (ex_a :: [*]) :: ([*],[*]) where
  UnionExs '(bs,ex_b) ex_a = '(bs, Union ex_a ex_b)

type family ConcatExs (bs_ex :: ([*],[*]))  (ex_a :: [*]) :: ([*],[*]) where
  ConcatExs '(bs,ex_b) ex_a = '(bs, Concat ex_a ex_b)
  
type family IsIDFunc (f :: * -> [*] -> ([*],[*]) -> *) :: Bool where
  IsIDFunc IDFunc = 'True
  IsIDFunc _ = 'False

type family ComposeFam' (isID :: Bool) (f_a :: * -> [*] -> ([*],[*]) -> *) (f_b :: * -> [*] -> ([*],[*]) -> *) (sp :: *) (as :: [*]) :: ([*],[*]) where   
  ComposeFam' True f_a f_b sp as = Eval (f_b sp as)
  ComposeFam' False f_a f_b sp as = ComposeFam (Eval (f_a sp as)) sp f_b

data BindFunc :: (* -> [*] -> ([*],[*]) -> *) ->  (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (BindFunc f_a f_b sp as) = Eval (ComposeFunc f_a f_b sp as)
  
data ComposeFunc :: (* -> [*] -> ([*],[*]) -> *) ->  (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (ComposeFunc f_a f_b sp as) = ComposeFam' (IsIDFunc f_a) f_a f_b sp as -- ComposeFam (Eval (f_a sp as)) sp f_b

type (:>>) f_a f_b = ComposeFunc f_a f_b
infixl 1 :>>

type (:>>=) f_a f_b = BindFunc f_a f_b
infixr 2 :>>=

data IffFunc :: (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (IffFunc f_a sp xs) = UnionRs (Eval (f_a sp xs)) xs

data IfJustFunc :: (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (IfJustFunc f_a sp xs) = UnionRs (Eval (f_a sp xs)) xs

data IfElseFunc :: (* -> [*] -> ([*],[*]) -> *) ->  (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (IfElseFunc f_a f_b sp xs) = UnionRsEx (Eval (f_a sp xs)) (Eval (f_b sp xs))

type family UnionRsEx (t1 :: ([*],[*])) (t2 :: ([*],[*])) :: ([*],[*]) where
  UnionTuple '(rs1, ex1) '(rs2, ex2) = '(Union rs1 rs2, Union ex1 ex2) 

type family CaptureFam (xs_ex_sub :: ([*],[*])) (f_sub :: * -> [*] -> ([*],[*]) -> *) (sp :: *)  (xs :: [*]) :: ([*],[*]) where
  CaptureFam '(xs_sub,ex_sub) f_sub sp xs = UnionRs (Eval (f_sub sp xs_sub)) ex_sub   
type family UnionRs (bs_ex :: ([*],[*]))  (ex_a :: [*]) :: ([*],[*]) where
  UnionRs '(bs,ex_b) ex_a = '(Union bs ex_a, ex_b)
data CaptureFunc :: * -> (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (CaptureFunc sp1 f_sub sp xs) = CaptureFam (ListSplitterRes2 sp1 xs) f_sub sp xs

type family UnionTuple xs_ex :: [*] where
  UnionTuple '(xs,ex) = Union xs ex

type family CaptureOrElseFam (xs_ex_sub :: ([*],[*])) (f_sub1 :: * -> [*] -> ([*],[*]) -> *) (f_sub2 :: * -> [*] -> ([*],[*]) -> *) (sp :: *)  (xs :: [*]) :: ([*],[*]) where
  CaptureOrElseFam '(xs_sub,ex_sub) f_sub1 f_sub2 sp xs = UnionRsEx (Eval (f_sub1 sp xs_sub)) (Eval (f_sub2 sp ex_sub))    
data CaptureOrElseFunc :: * -> (* -> [*] -> ([*],[*]) -> *) ->  (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (CaptureOrElseFunc sp1 f_sub1 f_sub2 sp xs) = CaptureOrElseFam (ListSplitterRes2 sp1 xs) f_sub1 f_sub2 sp xs

data EmbedFunc :: * -> (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (EmbedFunc sp1 f_sub sp xs) = ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 sp1 xs) f_sub (sp :&& sp1) xs))

data GetNewStateFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (GetNewStateFunc f sp xs) = GetNewStateFam  (Eval (f sp xs)) xs
type family GetNewStateFam (rs_ex :: ([*],[*])) (xs :: [*]) :: ([*],[*]) where 
  GetNewStateFam '(rs,ex) xs = '(FilterList xs rs, ex)

data IDFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (IDFunc sp xs) = '(xs,'[])

data WhatNextFunc :: * -> * -> [*] -> Exp ([*],[*])
type instance Eval (WhatNextFunc s sp xs) = '(xs,'[])

data WhatNamesFunc :: * -> * -> [*] -> Exp ([*],[*])
type instance Eval (WhatNamesFunc names sp xs) = '(xs,'[])

data WhatStepsFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (WhatStepsFunc sp xs) = '(xs,'[])

data QueryStateFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (QueryStateFunc sp xs) = '(xs,'[])

data NoopFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (NoopFunc sp xs) = '(xs,'[])

data LiftIOFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (LiftIOFunc sp xs) = '(xs,'[])

data MapFunc :: (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp ([*],[*])
type instance Eval (MapFunc f sp xs) = Eval (f sp xs)

data AskFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (AskFunc sp xs) = '(xs,'[])

data AsksFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (AsksFunc sp xs) = '(xs,'[])

data DictFunc :: Symbol -> * -> [*] -> Exp ([*],[*])
type instance Eval (DictFunc keyName sp xs) = '(xs,'[])

data SkipFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (SkipFunc sp xs) = Eval (NextStepsFunc (TotalSteps sp xs GetNextAllFunc) sp xs) 

data OpInterFunc :: ([*] -> (* -> *) -> Constraint) -> * -> [*] -> Exp ([*],[*])
type instance Eval (OpInterFunc c sp xs) = '(xs,'[])

type family ForeverFam (xs_ex :: ([*],[*])) (sp :: *) (xs :: [*]) :: ([*],[*]) where
  ForeverFam '(xs,ex) sp xs = '(('[]),ex)
  ForeverFam '(ys,ex) sp xs = 
    TypeError 
      ( 'Text "Forever input `"
        ':<>: 'ShowType xs ':<>: 'Text "' and output `"
        ':<>: 'ShowType ys ':<>: 'Text " dont match'")
  
data ForeverFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp  ([*],[*]) 
type instance Eval (ForeverFunc f sp xs) = ForeverFam (Eval (f sp xs)) sp xs

data WhileFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp  ([*],[*]) 
type instance Eval (WhileFunc f sp xs) = Eval (f sp xs)

data AlignFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (AlignFunc f sp xs) = '(xs, Second (Eval (f sp xs))) 

data ExtendForLoopFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (ExtendForLoopFunc f sp xs) = '(First (TransformLoop sp xs f), '[]) 

data ReplicateFunc :: n -> (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (ReplicateFunc n f sp xs) = ReplicateFam sp '(xs, '[]) f n  

type family ReplicateFam sp (xs :: ([*],[*])) (f :: * -> [*] -> Exp ([*],[*])) (n :: NatOne) :: ([*],[*]) where
  ReplicateFam sp '(xs,ex) f One = ConcatExs (Eval (f sp xs)) ex
  ReplicateFam sp '(xs,ex) f (Succ n) = ReplicateFam sp (ConcatExs (Eval (f sp xs)) ex) f n      

type family First ab  where
  First '(a,b) = a

type family Second ab  where
  Second '(a,b) = b

data ConstFunc :: [*] -> * -> [*] -> Exp ([*],[*])
type instance Eval (ConstFunc cs sp xs) = '(cs,'[])

type family TransformLoop sp (xs :: [*]) (f :: * -> [*] -> Exp ([*],[*])) :: ([*], [*]) where
  TransformLoop sp xs f = TransformLoop' sp (Eval (f sp xs)) '(xs,'[]) f    

type family TransformLoop' sp (nextXsEx :: ([*], [*])) (totalXsEx :: ([*], [*])) (f :: * -> [*] -> Exp ([*],[*])) ::  ([*], [*]) where
  TransformLoop' sp '(('[]), nextEx)  '(totalXs, totalEx) f = '(totalXs, Union totalEx nextEx)    
  TransformLoop' sp '(nextXs, nextEx) '(totalXs, totalEx) f = TransformLoop'' sp '(nextXs, nextEx)  (IsSublist totalXs nextXs) '(totalXs, totalEx) f     
  
type family TransformLoop'' sp (nextXs :: ([*],[*])) (isSublist :: Bool) (totalXs :: ([*],[*])) (f :: * -> [*] -> Exp ([*],[*])) :: ([*], [*]) where
  TransformLoop'' sp nextXs True sumXs f = sumXs    
  TransformLoop'' sp '(nextXs, nextEx) 'False '(totalXs, totalEx) f = TransformLoop' sp (Eval (f sp nextXs)) '(Union totalXs nextXs, Union totalEx nextEx)  f    

data TransformLoopFunc ::  (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp ([*],[*])
type instance Eval (TransformLoopFunc f sp xs) = TransformLoop sp xs f

type family TotalSteps sp (xs :: [*]) (f :: * -> [*] -> Exp ([*],[*])) :: NatOne where
  TotalSteps sp xs f = TotalSteps' sp (First(Eval (f sp xs))) f 'One     

type family TotalSteps' sp (xs :: [*]) (f :: * -> [*] -> Exp ([*],[*])) (steps :: NatOne) :: NatOne where
  TotalSteps' sp '[] f n = n     
  TotalSteps' sp xs f n = TotalSteps' sp (First(Eval (f sp xs))) f ('Succ n)    
  
data StepsFunc :: NatOne -> (* -> [*] -> ([*],[*]) -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (StepsFunc steps f sp xs) = Eval ((StepsFuncFam steps f) sp xs)

type family StepsFuncFam steps (f :: * -> [*] -> Exp ([*],[*])) :: (* -> [*] -> Exp ([*],[*])) where
  StepsFuncFam One f = f
  StepsFuncFam (Succ n) f = ComposeFunc (StepsFuncFam n f) f 

data NextStepsFunc :: NatOne -> * -> [*] -> Exp ([*],[*])
type instance Eval (NextStepsFunc steps sp xs) = Eval (StepsFuncFam steps GetNextAllFunc sp xs)


data ShowStateFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (ShowStateFunc sp xs) = 
  TypeError 
  ( 'Text "Current state `"
    ':<>: 'ShowType xs 
  )

data HandleLoopFunc :: (* -> [*] -> Exp ([*],[*])) -> (* -> [*] -> Exp ([*],[*]))
type instance Eval (HandleLoopFunc f sp xs) = 
  Eval ( 
    (ComposeFunc
      (ExtendForLoopFunc f) 
      (ForeverFunc (AlignFunc f))) sp xs)
  

data ScopeFunc ::  (* -> [*] -> Exp ([*],[*])) ->  ([*] -> [*] -> Exp (* -> [*] -> Exp ([*],[*]))) ->  * -> [*] -> Exp  ([*],[*]) 
type instance Eval (ScopeFunc f df sp xs) = 
    Eval ((ComposeFunc f (Eval (df xs (First (Eval (f sp xs)))))) sp xs)    


