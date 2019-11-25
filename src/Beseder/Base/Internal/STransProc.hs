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
import           Beseder.Base.Internal.STransDef

data Step (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*])
data OnStep (sp1 :: *) (steps :: [*])  
data OnOrStep (sp1 :: *) (stepsOn :: [*]) (stepsElse :: [*])  
data TryStep (sp1 :: *) (steps :: [*])  
data ForeverStep (steps :: [*])  
data LoopStep (steps :: [*])  

type family ApplyFunc (func :: * -> [*] -> Exp ([*],[*])) (sp :: *) (xs :: [*]) :: [*] where
  ApplyFunc (ComposeFunc f1 f2) sp xs = Concat (ApplyFunc f1 sp xs) (ApplyFunc f2 sp (First (Eval (f1 sp xs))))
  ApplyFunc (CaptureFunc sp1 f1) sp xs = '[OnStep sp1 (ApplyFunc f1 sp (ListSplitterRes sp1 xs))] 
  ApplyFunc (CaptureOrElseFunc sp1 f1 f2) sp xs = '[OnOrStep sp1 (ApplyFunc f1 sp (ListSplitterRes sp1 xs)) (ApplyFunc f2 sp (ListSplitterReminder sp1 xs))] 
  ApplyFunc (EmbedFunc sp1 f1) sp xs = '[TryStep sp1 (ApplyFunc f1 (sp :&& sp1) (ListSplitterRes sp1 xs))] 
  ApplyFunc (ForeverFunc f) sp xs = '[ForeverStep (ApplyFunc f sp xs)]
  ApplyFunc (ExtendForLoopFunc f) sp xs = '[]
  ApplyFunc (AlignFunc f) sp xs = ApplyFunc f sp xs
  ApplyFunc (HandleLoopFunc f) sp xs = 
    '[LoopStep (ApplyFunc 
                  (ComposeFunc
                    (ExtendForLoopFunc f) 
                    (ForeverFunc (AlignFunc f))) sp xs)]
  ApplyFunc func sp xs = '[Step func sp xs]
    
