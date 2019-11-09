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

-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.Base.Internal.STransDataDo where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransData

newRes :: Named name -> resPars -> STransData m sp (NewResFunc resPars name m) ()
newRes = NewRes

invoke :: Named name -> req -> STransData m sp (InvokeAllFunc req name) ()
invoke = Invoke
  
clear :: Named name -> STransData m sp (ClearAllFunc name) ()
clear = Clear

nextEv' :: STransData m sp GetNextAllFunc ()
nextEv' = NextEv'

termAndClearAllResources :: STransData m sp ClearAllVarFunc ()
termAndClearAllResources = ClearResources'

clearAllResources :: STransData m sp (CaptureFunc Statics ClearAllVarFunc) ()
clearAllResources = On @(Statics) ClearResources'

try :: forall sp1 sp m f_sub. STransData m (sp :&& sp1) f_sub () -> STransData m sp (EmbedFunc sp1 f_sub) ()
try = Try

on :: forall sp1 sp m f_sub. STransData m sp f_sub () -> STransData m sp (CaptureFunc sp1 f_sub) ()
on = On

opRes :: Named name -> (x -> m a) -> STransData m sp (OpResFunc name x) a
opRes = OpRes

return :: a -> STransData m sp (ReturnFunc a) a
return = Return

(>>=) :: STransData m sp f1 a -> (a -> STransData m sp f2 b) -> STransData m sp (BindFunc f1 f2) b
(>>=) = Bind

(>>) :: STransData m sp f1 () -> STransData m sp f2 b -> STransData m sp (ComposeFunc f1 f2) b
(>>) = Compose

liftIO :: IO a -> STransData m sp LiftIOFunc a
liftIO = LiftIO

noop :: STransData m sp NoopFunc ()
noop = Noop

nextEv :: STransData m sp (CaptureFunc Dynamics GetNextAllFunc) ()
nextEv = On @Dynamics nextEv'

nextSteps :: forall (steps :: Nat) m sp steps1. (steps1 ~ NatConv steps) => STransData m sp (CaptureFunc Dynamics (NextStepsFunc steps1)) ()
nextSteps = On @Dynamics (NextSteps (Proxy @steps1))

skipAll ::  STransData m sp (EmbedFunc Dynamics SkipFunc) () 
skipAll = Try @Dynamics Skip

skipTo ::  forall sp1 sp m. STransData m sp (EmbedFunc (Not sp1) SkipFunc) () 
skipTo = Try @(Not sp1) Skip

op :: m a -> STransData m sp OpFunc a
op = Op

forever :: STransData m sp f () -> STransData m sp (ForeverFunc f) ()
forever = Forever