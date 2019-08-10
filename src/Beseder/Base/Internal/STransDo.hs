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
{-# LANGUAGE RebindableSyntax      #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module Beseder.Base.Internal.STransDo 
  where

import           Protolude                hiding (Product, handle,return, lift, liftIO, (>>), (>>=), forever, First, try,on)
import           Control.Monad.Cont       hiding (return, lift, liftIO, (>>), (>>=), forever)
--import           Haskus.Utils.Flow
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Type.Errors hiding (Eval,Exp)
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.STrans
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.TypeExp
import           Beseder.Utils.ListHelper
import           Beseder.Utils.VariantHelper

withRes' ::
  ( CreateRes m name resFact (V '[res])
  , KnownSymbol name
  , AppendToTuple x res
  , SplicC sp rs ex zs
  , Show resFact
  , Liftable ys zs
  , Liftable '[AppendToTupleResult x res] zs
  , zs ~ Union '[AppendToTupleResult x res] ys
  , IsTypeUnique name x 
  --, '(rs,ex) ~ Eval (WithResFunc res sp (x ': ys)) --assert?
  ) => Named name -> resFact -> STrans q m sp (x ': ys) '(rs,ex) (WithResFunc res) ()
withRes' = WithResTrans

withRes ::
  ( CreateRes m name resFact (V '[res])
  , AppendToTuple (Variant xs) res
  , zs ~ AppendToTupleList xs res
  , SplicC sp rs ex zs
  , KnownSymbol name
  , Show resFact
  , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
  , IsTypeUniqueList name xs 
  --, '(rs,ex) ~ Eval (WithResAllFunc res sp xs) --assert
  ) => Named name -> resFact -> STrans  q m sp xs '(rs,ex) (WithResAllFunc res) ()
withRes = WithResAllTrans

newRes :: 
  ( MkRes m resPars
  , res ~ St (ResSt resPars m) name
  , zs ~ AppendToTupleList xs res
  , SplicC sp rs ex zs
  , Show resPars
  , KnownSymbol name
  , AppendToTuple (Variant xs) res
  , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
  , IsTypeUniqueList name xs 
  ) => Named name -> resPars -> STrans  q m sp xs '(rs,ex) (NewResFunc resPars name m) ()
newRes = NewResTrans  

invoke ::
  ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
  , Show req
  , KnownSymbol name
  , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  -- , WhenStuck (ReqResult (NamedRequest req name) (VWrap xs NamedTuple)) (DelayError ('Text "No request supported detected" ))
  , SplicC sp rs ex zs
  ) => Named name -> req -> STrans q m sp xs '(rs,ex) (InvokeAllFunc req name) ()
invoke = InvokeAllTrans

clear ::
  ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , KnownSymbol name
  , SplicC sp rs ex zs
  ) => Named name -> STrans q m sp xs '(rs,ex) (ClearAllFunc name) ()
clear = ClearAllTrans

nextEvAll' ::
  ( Transition m (TransWrap xs) 
  , SplicC sp rs ex zs
  , zs ~ NextStates (TransWrap xs)
  ) => STrans (ContT Bool) m sp xs '(rs,ex) GetNextAllFunc ()
nextEvAll'= GetNextAllTrans

nextEv ::
  ( sp1 ~ By 'Dynamic
  , ListSplitter sp1 xs
  , '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
  , Transition m (TransWrap xs_sub) 
  , zs ~ NextStates (TransWrap xs_sub)
  , SplicC sp rs ex zs
  , _
  ) => STrans (ContT Bool) m sp xs '(Union rs ex_sub,ex) (CaptureFunc (By 'Dynamic) GetNextAllFunc) ()
nextEv = Beseder.Base.Internal.STransDo.on @(By 'Dynamic) GetNextAllTrans

termAndClearAllResources :: 
  ( TermState m (ClrVar xs), MonadTrans q
  , rs_ex ~ Eval (ClearAllVarFunc sp xs)
  ) => STrans q m sp xs rs_ex ClearAllVarFunc ()
termAndClearAllResources = ClearAllVarTrans

clearAllResources :: 
  ( sp1 ~ By 'Static
  , ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , TermState m (ClrVar xs_sub)
  , MonadTrans q
  , _
  ) => STrans q m sp xs '((Union '[()] ex_sub),'[]) (CaptureFunc (By 'Static) ClearAllVarFunc) ()
clearAllResources = 
  Beseder.Base.Internal.STransDo.on @(By 'Static) ClearAllVarTrans 

(>>) :: 
  ( ex_un ~ Union ex1 ex2
  , Liftable ex1 ex_un
  , Liftable ex2 ex_un
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b 
  ) => STrans q m sp as '(rs1,ex1) f_a () -> STrans q m sp rs1 '(rs2,ex2) f_b b -> STrans q m sp as '(rs2,ex_un) (ComposeFunc f_a f_b) b
(>>) = ComposeTrans  

infixl 1 >>

return :: a -> STrans q m sp xs '(xs, '[]) IDFunc a 
return = ReturnTrans

(>>=) ::
  ( ex_un ~ Union ex1 ex2 
  , Liftable ex1 ex_un
  , Liftable ex2 ex_un
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b
  ) => STrans q m sp as '(rs1,ex1) f_a a -> (a -> STrans q m sp rs1 '(rs2,ex2) f_b b) -> STrans q m sp as '(rs2,ex_un) (BindFunc f_a f_b) b
(>>=) = BindTrans 
infixl 1 >>=

capture ::  
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , GetInstance sp1
  , Eval (f_sub sp xs_sub) ~ '(rs_sub, ex) -- assert
  ) => sp1 -> STrans q m sp xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs1, ex) (CaptureFunc sp1 f_sub) ()
capture = CaptureTrans 

on :: forall sp1 sp xs xs_sub ex_sub rs1 rs_sub q m ex f_sub.
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , GetInstance sp1
  , Eval (f_sub sp xs_sub) ~ '(rs_sub, ex) -- assert
  ) => STrans q m sp xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs1, ex) (CaptureFunc sp1 f_sub) ()
on = capture (getInstance @sp1)

embed :: 
  ( sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , GetInstance sp1
  , Eval (f_sub (sp :&& sp1) (ListSplitterRes sp1 xs)) ~ '(rs_sub, ex) --assert
  ) => sp1 -> STrans q m (sp :&& sp1) xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs,ex1) (EmbedFunc sp1 f_sub) ()
embed = EmbedTrans

try :: forall sp1 sp sp2 xs_sub ex_sub ex1 rs rs_sub ex zs xs q m f_sub. 
  ( sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , GetInstance sp1
  , Eval (f_sub (sp :&& sp1) (ListSplitterRes sp1 xs)) ~ '(rs_sub, ex) --assert
  ) => STrans q m (sp :&& sp1) xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs,ex1) (EmbedFunc sp1 f_sub) ()
try = embed (getInstance @sp1)

forever ::
  ( Eval (f sp xs) ~ '(xs,ex)
  ) => STrans q m sp xs '(xs,ex) f () -> STrans q m sp xs '(('[]),ex) (ForeverFunc f) ()
forever = ForeverTrans

while :: 
  ( Eval (f sp xs) ~ '(xs,ex)
  ) => STrans q m sp xs '(xs,ex) f Bool -> STrans q m sp xs '(xs,ex) f ()
while = WhileTrans  

whatNext :: STrans q m sp xs '(xs, '[]) IDFunc (Proxy xs) 
whatNext = WhatNextTrans 

noop :: STrans q m sp xs '(xs, '[]) IDFunc () 
noop = NoopTrans 

gets' :: 
  ( TT x (TargetByName name x)
  , GetTarget (TargetByName name x) (TypeByName name x)) 
  => Named name -> (TypeByName name x->a) -> STrans q m sp '[x] '(('[x]), '[]) IDFunc a
gets' named f = GetTrans (f .  getByName named)

gets ::
  ( GetTypeByNameVar name x xs
  ) => (GetTypeByNameVar name x xs) => Named name -> (x -> a) -> STrans q m sp xs '(xs, '[]) IDFunc a
gets = GetAllTrans  

op :: 
  ( Monad m
  ) => m a -> STrans q m sp xs '(xs, '[]) IDFunc a
op = OpAllTrans

opRes :: 
  ( Monad m
  , GetTypeByNameVar name x xs
  ) => Named name -> (x -> m a) -> STrans q m sp xs '(xs, '[]) IDFunc a
opRes = OpResAllTrans

liftIO ::
  ( MonadIO m
  ) => IO a -> STrans q m sp xs '(xs, '[]) IDFunc a
liftIO = LiftIOTrans  

ifElse ::
  ( ex ~ Union ex1 ex2 
  , Liftable ex1 ex
  , Liftable ex2 ex
  , rs ~ Union rs1 rs2
  , Liftable rs1 rs
  , Liftable rs2 rs
  , Eval (f1 sp xs) ~ '(rs1, ex1)  --assert 
  , Eval (f2 sp xs) ~ '(rs2, ex2) --assert
  ) => Bool -> STrans q m sp xs '(rs1,ex1) f1 ()  -> STrans q m sp xs '(rs2,ex2) f2 () -> STrans q m sp xs '(rs, ex) (IfElseFunc f1 f2) ()  
ifElse = IfElseTrans

iff ::
  ( rs ~ Union rs1 xs
  , Liftable rs1 rs
  , Liftable xs rs
  , Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
  ) => Bool -> STrans q m sp xs '(rs1,ex) f1 ()  -> STrans q m sp xs '(rs, ex) (IffFunc f1) ()  
iff = IffTrans

alignWithLoop :: 
  ( TransformLoop sp xs f ~ '(rs, ex)
  , Liftable xs rs
  ) => STrans q m sp xs '(rs,ex) (TransformLoopFunc f) ()    
alignWithLoop = ExtendWithFuncTrans

alignWithLoopHandler :: 
  ( TransformLoop sp xs f_h ~ '(rs, ex)
  , Liftable xs rs
  ) => STrans q m sp rs '(rs,ex) f_h () -> STrans q m sp xs '(rs, '[]) (TransformLoopFunc f_h) ()
alignWithLoopHandler hnd = ExtendWithFuncTrans_

getStransFunc :: STrans q m sp xs '(rs,ex) f a -> STrans q m sp ys '(ys,'[]) IDFunc (Proxy f)
getStransFunc t = return Proxy

handleLoop ::
  ( loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , MonadTrans q
  , Monad (q m)
  , Liftable xs (First (TransformLoop' sp (Eval (f sp xs)) '(xs, '[]) f))
  , Liftable ex ex
  , _
  --, LiftVariant ex ex
  --, IsSubset '[] ex ~ 'True
  ) => STrans q m sp loopRes '(rs,ex) f () -> STrans q m sp xs '(('[]), ex) _ ()
handleLoop hnd = do
  px <- whatNext
  spx <- WhatSplitterTrans
  loopState <- getLoopFuncRes px spx hnd 
  ExtendTo loopState
  Beseder.Base.Internal.STransDo.forever $ do
    AlignTrans hnd

handleLoop' ::
  ( loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , MonadTrans q
  , Monad (q m)
  , Liftable xs loopRes
  , _
  ) => STrans q m sp loopRes '(rs,ex) f () -> STrans q m sp xs '(('[]), ex) _ ()
handleLoop' hnd = do
  ExtendForLoop hnd
  Beseder.Base.Internal.STransDo.forever $ do
    AlignTrans hnd
  
handleLoopDry ::
  ( loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , _
  ) => STrans q m sp loopRes '(rs,ex) f () -> STrans q m sp xs '(loopRes,'[]) _ ()
handleLoopDry hnd = do
  px <- whatNext
  spx <- WhatSplitterTrans
  loopState <- getLoopFuncRes px spx hnd 
  ExtendTo loopState

pumpEvents' ::
  ( f ~ GetNextAllFunc
  , loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , _
  ) => STrans (ContT Bool) m sp xs '(('[]), ex) _ ()
pumpEvents' = -- do
    handleLoop nextEvAll'

pumpEvents ::
  ( sp2 ~ (sp :&& sp1) 
  , sp1 ~ Dynamics
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union ex_sub ex
  , Liftable ex zs
  , Liftable ex_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , f ~ GetNextAllFunc
  , loopRes ~ First (TransformLoop sp2 xs_sub f)
  , Eval (f sp2 loopRes) ~ '(rs',ex)
  , f_sub ~ BindFunc
              IDFunc
              (BindFunc
                IDFunc
                (BindFunc
                    IDFunc
                    (ComposeFunc
                      (ConstFunc loopRes)
                      (ForeverFunc (AlignFunc GetNextAllFunc)))))
  , _
  ) => STrans (ContT Bool) m sp xs '(rs,ex1) (EmbedFunc sp1 f_sub) ()
pumpEvents = 
  Beseder.Base.Internal.STransDo.try @(Dynamics) $ 
    pumpEvents'

handleEvents' ::
  ( f ~ ComposeFunc GetNextAllFunc f_hnd
  , loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , _
  ) => STrans (ContT Bool) m sp _ _ f_hnd () -> STrans (ContT Bool) m sp xs '(('[]), ex) _ ()
handleEvents' hnd = 
    handleLoop $ do 
      nextEvAll'
      hnd

handleEvents ::
  ( sp2 ~ (sp :&& sp1) 
  , sp1 ~ Dynamics
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union ex_sub ex
  , Liftable ex zs
  , Liftable ex_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , f ~ ComposeFunc GetNextAllFunc f_hnd
  , loopRes ~ First (TransformLoop sp2 xs_sub f)
  , Eval (f sp2 loopRes) ~ '(rs',ex)
  , f_sub ~ BindFunc
              IDFunc
              (BindFunc
                IDFunc
                (BindFunc
                    IDFunc
                    (ComposeFunc
                      (ConstFunc loopRes)
                      (ForeverFunc (AlignFunc (ComposeFunc GetNextAllFunc f_hnd))))))
  , _ 
  ) => STrans (ContT Bool) m sp2 _ _ f_hnd () -> STrans (ContT Bool) m sp xs '(rs,ex1) (EmbedFunc sp1 f_sub) ()
handleEvents hnd = 
  Beseder.Base.Internal.STransDo.try @(Dynamics) $ 
    handleEvents' hnd
          
getFuncRes :: Proxy (f :: * -> [*] -> Exp ([*],[*])) -> STrans q m sp xs '(xs, '[]) IDFunc (Proxy (Eval (f sp xs)))
getFuncRes _ = return Proxy
    
getLoopFuncRes :: 
 ( loopRes ~ First (TransformLoop sp xs f)
 , Eval (f sp loopRes) ~ '(rs,ex) 
 ) => Proxy xs -> Proxy sp -> STrans q m sp loopRes '(rs,ex) f () -> STrans q m sp xs '(xs, '[]) IDFunc (Proxy loopRes) -- (First (TransformLoop sp xs f)))
getLoopFuncRes _ _ _ = return Proxy

