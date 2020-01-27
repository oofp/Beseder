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
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.Base.Internal.STransDataDo where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Common
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

onOrElse :: forall sp1 sp m f_sub1 f_sub2. STransData m sp f_sub1 () -> STransData m sp f_sub2 () -> STransData m sp (CaptureOrElseFunc sp1 f_sub1 f_sub2) ()
onOrElse = OnOrElse

class CaseOf f_sub2 where
  type CaseOfFunc f_sub2 :: * -> [*] -> Exp ([*],[*])
  caseOf :: forall sp1 sp m f_sub1. STransData m sp (ComposeFunc (CaptureFunc sp1 f_sub1) f_sub2) () -> 
    STransData m sp (CaptureOrElseFunc sp1 f_sub1 (CaseOfFunc f_sub2)) ()

instance CaseOf NoopFunc where
  type CaseOfFunc NoopFunc = NoopFunc
  caseOf (Compose (On sd1) sd2) = onOrElse sd1 sd2

instance CaseOf (AssertFunc sp) where
  type CaseOfFunc (AssertFunc sp) = AssertFunc sp
  caseOf (Compose (On sd1) sd2) = onOrElse sd1 sd2

instance CaseOf (BlockFunc nm f) where
  type CaseOfFunc (BlockFunc nm f) = BlockFunc nm f
  caseOf (Compose (On sd1) sd2) = onOrElse sd1 sd2
    
instance CaseOf f_sub3 => CaseOf (ComposeFunc (CaptureFunc sp2 f_sub2) f_sub3) where
  type CaseOfFunc (ComposeFunc (CaptureFunc sp2 f_sub2) f_sub3) = CaptureOrElseFunc sp2 f_sub2 (CaseOfFunc f_sub3) 
  caseOf (Compose (On sd1) sd2) = onOrElse sd1 (caseOf sd2)

endCase :: STransData m sp (AssertFunc EmptyList) ()  
endCase = Assert @EmptyList

noNamed :: Named ""
noNamed = Named

defCase :: STransData m sp f () -> STransData m sp (BlockFunc "" f) ()  
defCase = Block noNamed

block :: STransData m sp f () -> STransData m sp (BlockFunc "" f) ()  
block = Block noNamed

lb :: Named label -> STransData m sp f () -> STransData m sp (BlockFunc label f) ()  
lb = Block

assert :: forall sp1 sp m. STransData m sp (AssertFunc sp1) ()
assert = Assert

opRes :: Named name -> (x -> m a) -> STransData m sp (OpResFunc name x) a
opRes = OpRes

gets :: Named name -> (St x name -> a) -> STransData m sp (GetFunc name (St x name)) a
gets = Gets

--whatNext :: STransData m sp (WhatNextFunc s) s
--whatNext = WhatNext

--whatNames :: STransData m sp (WhatNamesFunc names) names
--whatNames = WhatNames

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

label :: Named label -> STransData m sp (LabelFunc label) ()
label  = Label

nextEv :: STransData m sp (CaptureFunc Dynamics GetNextAllFunc) ()
nextEv = On @Dynamics nextEv'

nextSteps :: forall (steps :: Nat) m sp steps1. (steps1 ~ NatConv steps) => STransData m sp (CaptureFunc Dynamics (NextStepsFunc steps1)) ()
nextSteps = On @Dynamics (NextSteps (Proxy @steps1))

skipAll ::  STransData m sp (EmbedFunc Dynamics SkipFunc) () 
skipAll = Try @Dynamics Skip

skipTo ::  forall sp1 sp m. STransData m sp (EmbedFunc (Not sp1) SkipFunc) () 
skipTo = Try @(Not sp1) Skip

op :: m a -> STransData m sp (OpFunc a) a
op = Op

forever :: STransData m sp f () -> STransData m sp (ForeverFunc f) ()
forever = Forever

while :: STransData m sp f Bool -> STransData m sp (WhileFunc f) ()
while = While

newState :: STransData m sp f () -> STransData m sp (GetNewStateFunc f) ()
newState = NewState

type HandleEventsFunc f =
    EmbedFunc Dynamics
        (HandleLoopFunc
            (ComposeFunc (CaptureFunc Dynamics GetNextAllFunc) f))

handleEvents :: STransData m (sp :&& Dynamics) f () -> STransData m sp (HandleEventsFunc f) () 
handleEvents hnd = 
    Try @Dynamics (HandleLoop (Compose nextEv hnd))


type PumpEventsFunc =  EmbedFunc Dynamics (HandleLoopFunc GetNextAllFunc)   
pumpEvents :: STransData m sp PumpEventsFunc () 
pumpEvents = Try @Dynamics (HandleLoop NextEv')

ifElse :: Bool -> STransData m sp f1 () -> STransData m sp f2 () -> STransData m sp (IfElseFunc f1 f2) ()  
ifElse = IfElse

iff :: Bool -> STransData m sp f1 () -> STransData m sp (IffFunc f1) ()  
iff = Iff
    
handleTo :: forall sp1 sp f m. STransData m ((sp :&& (Not sp1)) :&& Dynamics) f () -> STransData m sp (EmbedFunc (Not sp1) (HandleEventsFunc f)) ()
handleTo hnd = 
  Try @(Not sp1) (handleEvents hnd) 


