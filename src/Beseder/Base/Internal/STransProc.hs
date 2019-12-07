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
  ApplyWithFilter fltr (BindFunc f1 f2) sp xs = Concat (ApplyFunc f1 sp xs) (ApplyFunc f2 sp (First (Eval (f1 sp xs))))
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
  