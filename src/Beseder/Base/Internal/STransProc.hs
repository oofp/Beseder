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

module Beseder.Base.Internal.STransProc where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.StHelper
import           Beseder.Utils.ListHelper
import           GHC.TypeLits
import           Data.Text

data Step (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*])
data OnStep (sp1 :: *) (steps :: [*])  
data OnOrStep (sp1 :: *) (stepsOn :: [*]) (stepsElse :: [*])  
data TryStep (sp1 :: *) (steps :: [*])  
data ForeverStep (steps :: [*])  
data BlockStep (steps :: [*])  
data LoopStep (steps :: [*])  
data LabelStep (label :: Symbol) (sp :: *) (xs :: [*])  
data ErrorStep (func :: * -> [*] -> Exp ([*],[*])) (errState :: *) (errText :: Symbol) 

type family ApplyFunc (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where
  ApplyFunc (ComposeFunc f1 f2) sp xs = Concat (ApplyFunc f1 sp xs) (ApplyFunc f2 sp (First (Eval (f1 sp xs))))
  ApplyFunc (BindFunc f1 f2) sp xs = Concat (ApplyFunc f1 sp xs) (ApplyFunc f2 sp (First (Eval (f1 sp xs))))
  ApplyFunc (CaptureFunc sp1 f1) sp xs = '[OnStep sp1 (ApplyFunc f1 sp (ListSplitterRes sp1 xs))] 
  ApplyFunc (CaptureOrElseFunc sp1 f1 f2) sp xs = '[OnOrStep sp1 (ApplyFunc f1 sp (ListSplitterRes sp1 xs)) (ApplyFunc f2 sp (ListSplitterReminder sp1 xs))] 
  ApplyFunc (EmbedFunc sp1 f1) sp xs = '[TryStep sp1 (ApplyFunc f1 (sp :&& sp1) (ListSplitterRes sp1 xs))] 
  ApplyFunc (ForeverFunc f) sp xs = '[ForeverStep (ApplyFunc f sp xs)]
  ApplyFunc (BlockFunc f) sp xs = '[BlockStep (ApplyFunc f sp xs)]
  ApplyFunc (ExtendForLoopFunc f) sp xs = '[]
  ApplyFunc (AlignFunc f) sp xs = ApplyFunc f sp xs
  ApplyFunc (ScopeFunc f _) sp xs = ApplyFunc f sp xs
  ApplyFunc (HandleLoopFunc f) sp xs = 
    '[LoopStep (ApplyFunc 
                  (ComposeFunc
                    (ExtendForLoopFunc f) 
                    (ForeverFunc (AlignFunc f))) sp xs)]
  ApplyFunc (LabelFunc label) sp xs = '[LabelStep label sp xs]
  ApplyFunc func sp xs = '[Step func sp xs]
    

type family ApplyWithFilter (fltr :: (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool) (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where
  ApplyWithFilter fltr (ComposeFunc f1 f2) sp xs = Concat (ApplyWithFilter fltr f1 sp xs) (ApplyWithFilter fltr f2 sp (First (Eval (f1 sp xs))))
  ApplyWithFilter fltr (BindFunc f1 f2) sp xs = Concat (ApplyWithFilter fltr f1 sp xs) (ApplyWithFilter fltr f2 sp (First (Eval (f1 sp xs))))
  ApplyWithFilter fltr (CaptureFunc sp1 f1) sp xs = '[OnStep sp1 (ApplyWithFilter fltr f1 sp (ListSplitterRes sp1 xs))] 
  ApplyWithFilter fltr (CaptureOrElseFunc sp1 f1 f2) sp xs = '[OnOrStep sp1 (ApplyWithFilter fltr f1 sp (ListSplitterRes sp1 xs)) (ApplyWithFilter fltr f2 sp (ListSplitterReminder sp1 xs))] 
  ApplyWithFilter fltr (EmbedFunc sp1 f1) sp xs = '[TryStep sp1 (ApplyWithFilter fltr f1 (sp :&& sp1) (ListSplitterRes sp1 xs))] 
  ApplyWithFilter fltr (BlockFunc f) sp xs = '[BlockStep (ApplyWithFilter fltr f sp xs)]
  ApplyWithFilter fltr (ForeverFunc f) sp xs = '[ForeverStep (ApplyWithFilter fltr f sp xs)]
  ApplyWithFilter fltr (ExtendForLoopFunc f) sp xs = '[]
  ApplyWithFilter fltr (AlignFunc f) sp xs = ApplyWithFilter fltr f sp xs
  ApplyWithFilter fltr (ScopeFunc f _) sp xs = ApplyWithFilter fltr f sp xs
  ApplyWithFilter fltr (HandleLoopFunc f) sp xs = 
    '[LoopStep (ApplyWithFilter fltr 
                  (ComposeFunc
                    (ExtendForLoopFunc f) 
                    (ForeverFunc (AlignFunc f))) sp xs)]
  ApplyWithFilter fltr func sp xs = ApplyFuncIfTrue (Eval (fltr func sp xs)) func sp xs 
    
type family ApplyFuncIfTrue (fl :: Bool) (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where
  ApplyFuncIfTrue 'True (LabelFunc name) sp xs = '[LabelStep name sp xs]
  ApplyFuncIfTrue 'True func sp xs = '[Step func sp xs]
  ApplyFuncIfTrue 'False func sp xs = '[]

type family ValidateFunc (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: (Bool, [*]) where
  ValidateFunc (ComposeFunc f1 f2) sp xs = ValidateCompose (ValidateFunc f1 sp xs) f1 f2 sp xs
  ValidateFunc (BindFunc f1 f2) sp xs = ValidateFunc (ComposeFunc f1 f2) sp xs
  ValidateFunc (CaptureFunc sp1 f1) sp xs = PropValRes (OnStep sp1) (ValidateFunc f1 sp (ListSplitterRes sp1 xs)) 
  ValidateFunc (CaptureOrElseFunc sp1 f1 f2) sp xs = ValidateOnOrElse (ValidateFunc f1 sp (ListSplitterRes sp1 xs)) sp1 f1 f2 sp xs 
  ValidateFunc (EmbedFunc sp1 f1) sp xs = PropValRes (TryStep sp1) (ValidateFunc f1 (sp :&& sp1) (ListSplitterRes sp1 xs)) 
  ValidateFunc (ForeverFunc f) sp xs = PropValRes ForeverStep (ValidateFunc f sp xs)
  ValidateFunc (BlockFunc f) sp xs = PropValRes BlockStep (ValidateFunc f sp xs)
  ValidateFunc (ExtendForLoopFunc f) sp xs = '( 'True, '[])
  ValidateFunc (AlignFunc f) sp xs = ValidateFunc f sp xs
  ValidateFunc (ScopeFunc f _) sp xs = ValidateFunc f sp xs
  ValidateFunc (HandleLoopFunc f) sp xs = 
    PropValRes BlockStep 
      (ValidateFunc 
        (ComposeFunc
          (ExtendForLoopFunc f) 
          (ForeverFunc (AlignFunc f))) sp xs)
  ValidateFunc (LabelFunc label) sp xs = '( 'True, '[LabelStep label sp xs])
  ValidateFunc (InvokeAllFunc req name) sp xs = ValidateInvoke req name sp xs (ListContains (NamedRequest req name) (StReqs (V xs))) 
  ValidateFunc func sp xs = '( 'True, '[Step func sp xs])

type family PropValRes (wr :: [*] -> *) (res :: (Bool, [*])) :: (Bool, [*]) where
  PropValRes wr '(fl, steps) = '(fl, '[wr steps])

type family ValidateCompose (res1 :: (Bool, [*])) (func1 :: * -> [*] -> Exp ([*],[*])) (func2 :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: (Bool, [*]) where
  ValidateCompose '( 'False, steps1) f1 f2 sp xs = '( 'False, steps1)
  ValidateCompose '( 'True, steps1) f1 f2 sp xs = ValidateCompose' steps1 (ValidateFunc f2 sp (First (Eval (f1 sp xs)))) 

type family ValidateCompose' (steps1 :: [*]) (res2 :: (Bool, [*])) :: (Bool, [*]) where
  ValidateCompose steps1 '(fl, steps2) = '(fl, Concat steps1 steps2)

type family ValidateOnOrElse (res1 :: (Bool, [*])) (sp1 :: *) (func1 :: * -> [*] -> Exp ([*],[*])) (func2 :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: (Bool, [*]) where
  ValidateOnOrElse '( 'False, steps1) sp1 f1 f2 sp xs = '( 'False, '[OnOrStep sp1 steps1 '[]])
  ValidateOnOrElse '( 'True, steps1) sp1 f1 f2 sp xs = ValidateOnOrElse' sp1 steps1 (ValidateFunc f2 sp (ListSplitterReminder sp1 xs))

type family ValidateOnOrElse' (sp1 :: *) (steps1 :: [*]) (res2 :: (Bool, [*])) :: (Bool, [*]) where
  ValidateOnOrElse' sp1 steps1 '(fl, steps2) = '(fl, '[OnOrStep sp1 steps1 steps2])

type family ValidateInvoke (req :: *) (name :: Symbol) (sp :: *) (xs :: [*]) (fl :: Bool) :: (Bool, [*]) where
  ValidateInvoke req name sp xs 'True = '( 'True, '[Step (InvokeAllFunc req name) sp xs])
  ValidateInvoke req name sp xs 'False = '( 'False, '[ErrorStep (InvokeAllFunc req name) (V xs) "Request not supported"])

--

data LabelsOnly :: (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool
type instance Eval (LabelsOnly func sp xs) = IsLabelFam func  
type family IsLabelFam (func :: * -> [*] -> Exp ([*],[*])) :: Bool where
  IsLabelFam (LabelFunc _) = 'True  
  IsLabelFam _ = 'False

data LabelsName :: Symbol -> (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool
type instance Eval (LabelsName name func sp xs) = IsLabelNameFam name func  
type family IsLabelNameFam (name :: Symbol) (func :: * -> [*] -> Exp ([*],[*])) :: Bool where
  IsLabelNameFam name (LabelFunc name) = 'True   
  IsLabelNameFam _ _ = 'False
  
data MatchFunc :: (* -> [*] -> Exp ([*],[*])) -> (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool
type instance Eval (MatchFunc func1 func sp xs) = AreFuncMatchingFam func1 func  
type family AreFuncMatchingFam (func1 :: * -> [*] -> Exp ([*],[*])) (func :: * -> [*] -> Exp ([*],[*])) :: Bool where
  AreFuncMatchingFam f f = 'True  
  AreFuncMatchingFam f f1 = 'False  

type family GetSteps (d :: *) :: [*] where
  GetSteps (OnStep sp1 steps) = FlattenSteps steps
  GetSteps (OnOrStep sp1 steps1 steps2) = Concat (FlattenSteps steps1) (FlattenSteps steps2)
  GetSteps (TryStep sp1 steps) = FlattenSteps steps  
  GetSteps (ForeverStep steps) = FlattenSteps steps  
  GetSteps (BlockStep steps) = FlattenSteps steps  
  GetSteps (LoopStep steps) = FlattenSteps steps  
  GetSteps step = '[step]
  
type family FlattenSteps (steps :: [*]) :: [*] where
  FlattenSteps '[] = '[]
  FlattenSteps (s ': steps) = Concat (GetSteps s) (FlattenSteps steps)


--
data Edge (func :: * -> [*] -> Exp ([*],[*])) (fromState :: *) (toState :: *)

type family Edges (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where
  Edges (ComposeFunc f1 f2) sp xs = Concat (Edges f1 sp xs) (Edges f2 sp (First (Eval (f1 sp xs))))
  Edges (BindFunc f1 f2) sp xs = Edges (ComposeFunc f1 f2) sp xs
  Edges (CaptureFunc sp1 f1) sp xs = Edges f1 sp (ListSplitterRes sp1 xs) 
  Edges (CaptureOrElseFunc sp1 f1 f2) sp xs = Concat (Edges f1 sp (ListSplitterRes sp1 xs)) (Edges f2 sp (ListSplitterReminder sp1 xs)) 
  Edges (EmbedFunc sp1 f1) sp xs = Edges f1 (sp :&& sp1) (ListSplitterRes sp1 xs) 
  Edges (ForeverFunc f) sp xs = Edges f sp xs
  Edges (BlockFunc f) sp xs = Edges' f sp xs
  Edges (ExtendForLoopFunc f) sp xs = '[]
  Edges (AlignFunc f) sp xs = Edges f sp xs
  Edges (ScopeFunc f _) sp xs = Edges f sp xs
  Edges (HandleLoopFunc f) sp xs = 
    Edges' f sp (First (TransformLoop sp xs f))
  {-  
    Edges 
      (ComposeFunc
        (ExtendForLoopFunc f) 
          (ForeverFunc (AlignFunc f))) sp xs
  -}        
  Edges func sp xs = Edges' func sp xs

type family Edges' (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where  
  Edges' func sp '[] = '[]  
  Edges' func sp (x ': xs) = Concat (Edges2 func x (Eval (func sp '[x]))) (Edges' func sp xs)

type family Edges2 (func :: * -> [*] -> Exp ([*],[*])) x (xs_ex :: ([*],[*])) :: [*] where
  Edges2 func x '(xs, ex) = Edges3 func x (Concat xs ex)

type family Edges3 (func :: * -> [*] -> Exp ([*],[*])) x (xs_ex :: [*]) :: [*] where
  Edges3 func x '[] = '[]
  Edges3 (LabelFunc name) x '[x] = '[Edge (LabelFunc name) x x] 
  Edges3 func x (x ': xs) = Edges3 func x xs
  Edges3 func x (y ': xs) = (Edge func x y) ': (Edges3 func x xs)
  
data StatesAndLabels (sts :: [*]) (labels :: [(*,Symbol)])  
type family GetStatesAndLabels (edges :: [*]) :: * where
  GetStatesAndLabels edges = PostProcessStatesLabels (GetStatesAndLabels' edges)

type family GetStatesAndLabels' (edges :: [*]) :: * where
  GetStatesAndLabels' '[] = StatesAndLabels '[] '[]  
  GetStatesAndLabels' ((Edge (LabelFunc name) from to) ': moreEdges) = AddLabel name from (GetStatesAndLabels' moreEdges) 
  GetStatesAndLabels' ((Edge otherFunc () ()) ': moreEdges) = GetStatesAndLabels' moreEdges
  GetStatesAndLabels' ((Edge otherFunc from ()) ': moreEdges) = AddState from (GetStatesAndLabels' moreEdges)
  GetStatesAndLabels' ((Edge otherFunc () to) ': moreEdges) = AddState to (GetStatesAndLabels' moreEdges)
  GetStatesAndLabels' ((Edge otherFunc from to) ': moreEdges) = AddState from (AddState to (GetStatesAndLabels' moreEdges))

type family AddLabel (name :: Symbol) (s :: *) (stsLabels :: *) :: * where  
  AddLabel name s (StatesAndLabels sts lbls) = StatesAndLabels sts ( '(s, name) ': lbls) 

type family AddState (s :: *) (stsLabels :: *) :: * where  
  AddState s (StatesAndLabels sts lbls) = StatesAndLabels (s ': sts) lbls
    
type family PostProcessStatesLabels (stsLabels :: *) :: * where
  PostProcessStatesLabels (StatesAndLabels sts lbls) = StatesAndLabels (Nub sts) lbls

data VBegin
data VEnd
data VLabel (name :: Symbol)
data VIndex (i :: Nat)
data (-->) (from :: *) (to :: *)

class ShowV a where
  showV :: Proxy a -> Text

instance ShowV VBegin where
  showV _ = "[*]"
instance ShowV VEnd where
  showV _ = "[*]"
instance KnownSymbol name => ShowV (VLabel name) where
  showV _ = pack $ symbolVal (Proxy @name)
instance KnownNat ix => ShowV (VIndex ix) where
  showV _ = show $ natVal (Proxy @ix)
instance ShowV ( '[]) where
  showV _ = ""
instance (ShowV v, ShowV vs) => ShowV (v ': vs) where
  showV _ = (showV (Proxy @v)) <> "\n" <> (showV (Proxy @vs))
instance (ShowV v, ShowV v1) => ShowV ((-->) v v1) where
  showV _ = (showV (Proxy @v)) <> " --> " <> (showV (Proxy @v1))
      
type family StatesToSymbol (edges :: [*]) :: Symbol where
  StatesToSymbol edges = StatesToSymbol' (GetStatesAndLabels edges)

type family StatesToSymbol' (stsLabels :: *) :: Symbol where
  StatesToSymbol' (StatesAndLabels sts lbls) = StatesListToSymbol 0 sts
  
type family StatesListToSymbol (ix :: Nat) (sts :: [*]) :: Symbol where
  StatesListToSymbol ix '[] = ""
  StatesListToSymbol ix (st ': moreStates) = AppendSymbol (StateToSymbol (NatToSymbol (ix+1)) st) (StatesListToSymbol (ix+1) moreStates)

type family StateToSymbol (ixSym :: Symbol) (st :: *) :: Symbol where
  StateToSymbol ix () = ""
  StateToSymbol ix (St s name) = AppendSymbol (AppendSymbol ix (AppendSymbol " : " (ShowState (St s name)))) "\n"
  StateToSymbol ix (s, smore) = AppendSymbol (StateToSymbol ix s) (StateToSymbol ix smore)

type family TransformEdges (edges :: [*]) :: [*] where
  TransformEdges edges = TransformEdges' edges (GetStatesAndLabels edges) 

type family TransformEdges' (edges :: [*]) (stsLabels :: *) :: [*] where
  TransformEdges' '[] stsLables = '[]
  TransformEdges' ((Edge (LabelFunc name) x x) ': edges) stsLabels = TransformEdges' edges stsLabels
  TransformEdges' ((Edge f from to) ': edges) stsLabels = 
      ((-->) (GetVertex 'False from stsLabels)  (GetVertex 'True to stsLabels)) ': (TransformEdges' edges stsLabels)

type family GetVertex (flEnd :: Bool) (s :: *) (stsLabels :: *) where
  GetVertex 'False () stsLabels = VBegin
  GetVertex 'True () stsLabels = VEnd
  GetVertex fl s (StatesAndLabels sts labels) = GetVertex' s sts (FindLabel s labels)      

type family GetVertex' (s :: *) (sts :: [*]) (findLabelRes :: *) where
  GetVertex' s sts (VLabel name) = VLabel name
  GetVertex' s sts () = VIndex (FindState 0 s sts)

type family FindLabel (s :: *) (lbls :: [(*,Symbol)]) :: * where
  FindLabel s '[] = ()
  FindLabel s ( '(s, name) ': moreLabels) = VLabel name
  FindLabel s ( '(s1, name) ': moreLabels) = FindLabel s moreLabels

type family FindState (ix :: Nat) (s :: *) (sts :: [*]) :: Nat where
  FindState ix s '[] = 0
  FindState ix s (s ': moreStates) = (ix+1)
  FindState ix s (s1 ': moreStates) = FindState (ix+1) s moreStates

type family NatToSymbol (n :: Nat) :: Symbol where 
  NatToSymbol 0 = "0"
  NatToSymbol 1 = "1"
  NatToSymbol 2 = "2"
  NatToSymbol 3 = "3"
  NatToSymbol 4 = "4"
  NatToSymbol 5 = "5"
  NatToSymbol 6 = "6"
  NatToSymbol 7 = "7"
  NatToSymbol 8 = "8"
  NatToSymbol 9 = "9"
  NatToSymbol n = AppendSymbol (NatToSymbol (Div n 10)) (NatToSymbol (Mod n 10))
  

type family EdgesToText (edges :: [*]) :: Symbol where
  EdgesToText '[] = ""
  EdgesToText '[e] = EdgeToText e
  EdgesToText (e ': moreEdges) = AppendSymbol (EdgeToText e) (AppendSymbol "\n" (EdgesToText moreEdges)) 

type family EdgeToText (edge :: *) :: Symbol where
  EdgesToText (v1 --> v2) = AppendSymbol (VertexToText v1) (AppendSymbol " --> " (VertexToText v2))

type family VertexToText (v :: *) :: Symbol where
  VertexToText VBegin = "[*]"
  VertexToText VEnd = "[*]"
  VertexToText (VIndex ix) = (NatToSymbol ix)
  VertexToText (VLabel l) = l
   
