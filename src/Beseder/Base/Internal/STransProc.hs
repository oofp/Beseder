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
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef

data Step (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*])
data OnStep (sp1 :: *) (steps :: [*])  
data OnOrStep (sp1 :: *) (stepsOn :: [*]) (stepsElse :: [*])  
data TryStep (sp1 :: *) (steps :: [*])  
data ForeverStep (steps :: [*])  
data BlockStep (steps :: [*])  
data LoopStep (steps :: [*])  
data LabelStep (label :: Symbol) (sp :: *) (xs :: [*])  

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
  Edges (BlockFunc f) sp xs = Edges f sp xs
  Edges (ExtendForLoopFunc f) sp xs = '[]
  Edges (AlignFunc f) sp xs = Edges f sp xs
  Edges (ScopeFunc f _) sp xs = Edges f sp xs
  Edges (HandleLoopFunc f) sp xs = 
    Edges 
      (ComposeFunc
        (ExtendForLoopFunc f) 
          (ForeverFunc (AlignFunc f))) sp xs
  Edges (LabelFunc label) sp xs = '[]
  Edges func sp xs = Edges' func xs

type family Edges' (func :: * -> [*] -> Exp ([*],[*])) (xs :: [*]) :: [*] where  
  Edges' func '[] = '[]  
  Edges' func  (x ': xs) = Concat (Edges'' func x (First (Eval (func NoSplitter '[x])))) (Edges' func xs)

type family Edges'' (func :: * -> [*] -> Exp ([*],[*])) x (xs :: [*]) :: [*] where
  Edges'' func x '[] = '[]
  Edges'' func x (x ': xs) = Edges'' func x xs
  Edges'' func x (y ': xs) = (Edge func x y) ': (Edges'' func x xs)

--
