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

module Beseder.Base.Internal.STransData where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransProc

data STransData (m :: * -> *) (sp :: *) (sfunc :: * -> [*] -> Exp ([*],[*])) (a :: *) where 
  Return :: a -> STransData m sp (ReturnFunc a) a
  Bind :: STransData m sp f1 a -> (a -> STransData m sp f2 b) -> STransData m sp (BindFunc f1 f2) b 
  Compose :: STransData m sp f1 () -> STransData m sp f2 b -> STransData m sp (ComposeFunc f1 f2) b 
  NewRes :: Named name -> resPars -> STransData m sp (NewResFunc resPars name m) ()
  Invoke :: Named name -> req -> STransData m sp (InvokeAllFunc req name) ()
  Clear :: Named name -> STransData m sp (ClearAllFunc name) ()
  NextEv' :: STransData m sp GetNextAllFunc ()
  RenameRes :: Named resName -> Named newName -> STransData m sp (RenameResFunc resName newName) ()
  ClearResources' :: STransData m sp ClearAllVarFunc ()
  Try :: forall sp1 sp m f_sub. STransData m (sp :&& sp1) f_sub () -> STransData m sp (EmbedFunc sp1 f_sub) ()
  On :: forall sp1 sp m f_sub. STransData m sp f_sub () -> STransData m sp (CaptureFunc sp1 f_sub) ()
  OnOrElse :: forall sp1 sp m f_sub1 f_sub2. STransData m sp f_sub1 () -> STransData m sp f_sub2 () -> STransData m sp (CaptureOrElseFunc sp1 f_sub1 f_sub2) ()
  Gets :: Named name -> (St x name -> a) -> STransData m sp (GetFunc name (St x name)) a
  Prop :: Named name -> Proxy propKey -> STransData m sp (GetPropFunc name propKey) (PropType propKey)
  OpRes :: Named name -> (x -> m a) -> STransData m sp (OpResFunc name x) a
  Op :: m a -> STransData m sp (OpFunc a) a
  Noop :: STransData m sp NoopFunc ()
  Label :: Named label -> STransData m sp (LabelFunc label) ()
  LiftIO :: IO a -> STransData m sp LiftIOFunc a
  NextSteps :: Proxy n -> STransData m sp (NextStepsFunc n) ()
  Forever :: STransData m sp f () -> STransData m sp (ForeverFunc f) ()
  While :: STransData m sp f Bool -> STransData m sp (WhileFunc f) ()
  NewState :: STransData m sp f () -> STransData m sp (GetNewStateFunc f) ()
  Skip :: STransData m sp SkipFunc ()
  HandleLoop :: STransData m sp f () -> STransData m sp (HandleLoopFunc f) () 
  IfElse :: Bool -> STransData m sp f1 () -> STransData m sp f2 () -> STransData m sp (IfElseFunc f1 f2) ()  
  Iff :: Bool -> STransData m sp f1 () -> STransData m sp (IffFunc f1) ()  
  Scope :: STransData m sp f () -> Proxy df -> STransData m sp (ScopeFunc f df) ()
  Func :: Proxy ff -> STransData m sp (FuncFunc ff) ()
  Block :: Named label -> STransData m sp f () -> STransData m sp (BlockFunc label f) ()
  Assert :: forall sp1 sp m. STransData m sp (AssertFunc sp1) ()
  --WhatNext :: STransData m sp (WhatNextFunc s) s
  --WhatNames :: STransData m sp (WhatNamesFunc names) names

evalSTransData' :: forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (Eval (f sp xs))
evalSTransData' _sd _ = Proxy

evalSTransData :: STransData m NoSplitter f a -> Proxy (Eval (f NoSplitter '[()]))
evalSTransData sd  = evalSTransData' sd (Proxy @('[()])) 

edgesSTransData' :: forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (Edges f sp xs)
edgesSTransData' _sd _ = Proxy

edgesSTransData :: STransData m NoSplitter f a -> Proxy (Edges f NoSplitter '[()])
edgesSTransData sd  = edgesSTransData' sd (Proxy @('[()])) 

statesAndLabels' :: forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (GetStatesAndLabels (Edges f sp xs))
statesAndLabels' _sd _ = Proxy

statesAndLabels :: STransData m NoSplitter f a -> Proxy (GetStatesAndLabels (Edges f NoSplitter '[()]))
statesAndLabels sd  = statesAndLabels' sd (Proxy @('[()])) 

vedgesSTransData' :: forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (TransformEdges (Edges f sp xs))
vedgesSTransData' _sd _ = Proxy

vedgesSTransData :: STransData m NoSplitter f a -> Proxy (TransformEdges (Edges f NoSplitter '[()]))
vedgesSTransData sd  = vedgesSTransData' sd (Proxy @('[()])) 

getSTransDiagram' ::  forall sp m f xs a edges. (TransformEdges (Edges f sp xs) ~ edges, ShowV edges) => STransData m sp f a -> Proxy xs -> Text
getSTransDiagram' _sd _ = showV (Proxy @(TransformEdges (Edges f sp xs)))

getSTransDiagram :: (TransformEdges (Edges f NoSplitter '[()]) ~ edges, ShowV edges) => STransData m NoSplitter f a -> Text
getSTransDiagram sd  = getSTransDiagram' sd (Proxy @('[()])) 

getSTransDiagramSymbol' ::  forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (EdgesToText (TransformEdges (Edges f sp xs)) "")
getSTransDiagramSymbol' _sd _ = Proxy @(EdgesToText (TransformEdges (Edges f sp xs)) "")

getSTransDiagramSymbol :: STransData m NoSplitter f a -> Proxy (EdgesToText (TransformEdges (Edges f NoSplitter '[()])) "")
getSTransDiagramSymbol sd  = getSTransDiagramSymbol' sd (Proxy @('[()])) 

getSTransDiagramStates' ::  forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (StatesToSymbol (Edges f sp xs) "")
getSTransDiagramStates' _sd _ = Proxy @(StatesToSymbol (Edges f sp xs) "")

getSTransDiagramStates ::  forall sp m f xs a. STransData m sp f a -> Proxy xs -> Proxy (StatesToSymbol (Edges f NoSplitter '[()]) "")
getSTransDiagramStates _sd _ = Proxy @(StatesToSymbol (Edges f NoSplitter '[()]) "")

evalSTransDataApp' :: STransData m sp f a -> Proxy xs -> Proxy (ApplyFunc f sp xs)
evalSTransDataApp' _sd_ _ = Proxy 

evalSTransDataApp :: STransData m NoSplitter f a -> Proxy (ApplyFunc f NoSplitter '[()])
evalSTransDataApp _sd_  = Proxy 

evalSTransDataLabels' :: STransData m sp f a -> Proxy xs -> Proxy (ApplyWithFilter LabelsOnly f sp xs)
evalSTransDataLabels' _sd_ _ = Proxy 

evalSTransDataLabels :: STransData m sp f a -> Proxy (ApplyWithFilter LabelsOnly f NoSplitter '[()])
evalSTransDataLabels _sd_ = Proxy 

validateSTransData' :: STransData m sp f a -> Proxy xs -> Proxy (ValidateFunc f sp xs)
validateSTransData' _sd_ _ = Proxy 

validateSTransData :: STransData m sp f a -> Proxy (ValidateFunc f NoSplitter '[()])
validateSTransData _sd_ = Proxy 

validateSteps' :: STransData m sp f a -> Proxy (labels :: [Symbol]) -> Proxy xs -> Proxy (ValidateSteps labels f sp xs)
validateSteps' _sd_ _ _ = Proxy 

validateSteps :: STransData m sp f a -> Proxy (labels :: [Symbol]) -> Proxy (ValidateSteps labels f sp '[()])
validateSteps _sd_ _ = Proxy 

getError' :: STransData m sp f a -> Proxy xs -> Proxy (ValidateSteps '[] f sp xs )
getError' _sd_ _ = Proxy 

getError :: STransData m sp f a -> Proxy (ValidateSteps '[] f sp '[()])
getError _sd_ = Proxy 

evalSTransDataNamedLabels' :: Named label -> STransData m sp f a -> Proxy xs -> Proxy (ApplyWithFilter (LabelsName label) f sp xs)
evalSTransDataNamedLabels' _ _sd_ _ = Proxy 

evalSTransDataNamedLabels :: Named label -> STransData m sp f a -> Proxy (ApplyWithFilter (LabelsName label) f NoSplitter '[()])
evalSTransDataNamedLabels _ _sd_ = Proxy 

evalSTransDataAppFiltered' :: Proxy (withFilter ::  (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool) -> STransData m sp f a -> Proxy xs -> Proxy (ApplyWithFilter withFilter f sp xs)
evalSTransDataAppFiltered' _ _sd_ _ = Proxy 

evalSTransDataAppFiltered :: Proxy (withFilter ::  (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp Bool) -> STransData m sp f a -> Proxy (ApplyWithFilter withFilter f NoSplitter '[()])
evalSTransDataAppFiltered _ _sd_ = Proxy 

flattenSteps :: Proxy (stepsTree :: [*])  -> Proxy (FlattenSteps stepsTree)
flattenSteps _ = Proxy

getLabel' :: Named label -> STransData m sp f a -> Proxy xs -> Proxy (FlattenSteps (ApplyWithFilter (LabelsName label) f sp xs))
getLabel' _ _ _ = Proxy

getLabel :: Named label -> STransData m sp f a -> Proxy (FlattenSteps (ApplyWithFilter (LabelsName label) f NoSplitter '[()]))
getLabel _ _ = Proxy

(>>>) :: STransData m sp f1 () -> STransData m sp f2 b -> STransData m sp (ComposeFunc f1 f2) b 
(>>>) = Compose
infixr 1 >>>

(>>>=) :: STransData m sp f1 a -> (a -> STransData m sp f2 b) -> STransData m sp (BindFunc f1 f2) b 
(>>>=) = Bind
infixr 1 >>>=


