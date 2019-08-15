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

module Beseder.Base.Internal.STrans where

import           Protolude                    hiding (Product, handle,TypeError,First)
import           Control.Monad.Cont
import           Control.Monad.Identity
import           Haskus.Utils.Flow
import           Data.Text
import           Data.Typeable
import           GHC.TypeLits
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Type.Errors hiding (Eval,Exp)

import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Utils.ListHelper
import           Beseder.Utils.VariantHelper
import           Beseder.Base.Internal.SplitFlow
import           Beseder.Utils.Lst

-- transformation defunc data and their evaluators
data WithResFunc :: res -> * -> [*] -> Exp ([*],[*])
type instance Eval (WithResFunc res sp (x ': ys)) = ListSplitterRes2 sp (Union '[AppendToTupleResult x res] ys)

data WithResAllFunc :: res -> * -> [*] -> Exp ([*],[*])
type instance Eval (WithResAllFunc res sp xs) = ListSplitterRes2 sp (AppendToTupleList xs res)

data NewResFunc :: (resPars :: *) -> name -> (* -> *) -> * -> [*] -> Exp ([*],[*])
type instance Eval (NewResFunc resPars name m sp xs) = ListSplitterRes2 sp (AppendToTupleList xs (St (ResSt m resPars) name))

data InvokeAllFunc :: (req :: *) -> name -> * -> [*] -> Exp ([*],[*])
type instance Eval (InvokeAllFunc req name sp xs) = ListSplitterRes2 sp (ReqResult (NamedRequest req name) (VWrap xs NamedTuple))

data ClearAllFunc :: name -> * -> [*] -> Exp ([*],[*])
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
  ComposeFam '(as1,ex1) sp f_b = UnionExs (Eval (f_b sp as1)) ex1
type family UnionExs (bs_ex :: ([*],[*]))  (ex_a :: [*]) :: ([*],[*]) where
  UnionExs '(bs,ex_b) ex_a = '(bs, Union ex_a ex_b)

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

data EmbedFunc :: * -> (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (EmbedFunc sp1 f_sub sp xs) = ListSplitterRes2 sp (UnionTuple (CaptureFam (ListSplitterRes2 sp1 xs) f_sub (sp :&& sp1) xs))

data IDFunc :: * -> [*] -> Exp ([*],[*])
type instance Eval (IDFunc sp xs) = '(xs,'[])

data DictFunc :: Symbol -> * -> [*] -> Exp ([*],[*])
type instance Eval (DictFunc keyName sp xs) = '(xs,'[])

type family ForeverFam (xs_ex :: ([*],[*])) (sp :: *) (xs :: [*]) :: ([*],[*]) where
  ForeverFam '(xs,ex) sp xs = '(('[]),ex)
  ForeverFam '(ys,ex) sp xs = 
    TypeError 
      ( 'Text "Forever input `"
        ':<>: 'ShowType xs ':<>: 'Text "' and output `"
        ':<>: 'ShowType ys ':<>: 'Text " dont match'")
  
data ForeverFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp  ([*],[*]) 
type instance Eval (ForeverFunc f sp xs) = ForeverFam (Eval (f sp xs)) sp xs

data AlignFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (AlignFunc f sp xs) = '(xs, Second (Eval (f sp xs))) 

data ExtendForLoopFunc :: (* -> [*] -> ([*],[*]) -> *) ->  * -> [*] -> Exp ([*],[*])
type instance Eval (ExtendForLoopFunc f sp xs) = '(First (TransformLoop sp xs f), '[]) 

type family First ab  where
  First '(a,b) = a

type family Second ab  where
  Second '(a,b) = b

data ConstFunc :: [*] -> * -> [*] -> Exp ([*],[*])
type instance Eval (ConstFunc cs sp xs) = '(cs,'[])

-- Constrains
type family ComposeCFam sp f_a as as1 f_b bs zs where 
  ComposeCFam sp f_a as as1 f_b bs zs =     
    ( ListSplitter sp as1
    , bs ~ ListSplitterRes sp as1 -- right 
    , Liftable (Eval (f_b sp bs)) zs
    , Liftable (FilterList bs as1) zs
    , VariantSplitter bs (FilterList bs as1) as1
    )


type SplicC sp xs ex zs = 
  ( ListSplitter sp zs
  , VariantSplitter xs ex zs
  , '(xs,ex) ~ ListSplitterRes2 sp zs
  )


class TransDict (q :: (* -> *) -> * -> *) (m :: * -> *) dict (key :: Symbol) (xs :: [*]) (a :: *) | q m xs dict key -> a where 
  getTransFromDict  :: dict -> Named key -> STransApp q m sp xs '(xs,'[]) a

--
data STrans q (m :: * -> *) (sp :: *) (xs :: [*]) (rs_ex :: ([*],[*])) (sfunc :: * -> [*] -> ([*],[*]) -> *) (a :: *) where
  WithResTrans ::
    ( CreateRes m name resFact (V '[res])
    , KnownSymbol name
    , AppendToTuple x res
    , SplicC sp rs ex zs
    , Show resFact
    , Liftable ys zs
    , Liftable '[AppendToTupleResult x res] zs
    , zs ~ Union '[AppendToTupleResult x res] ys
    , IsTypeUnique name x 
    -- , '(rs,ex) ~ Eval (WithResFunc res sp (x ': ys)) --assert
    ) => Named name -> resFact -> STrans q m sp (x ': ys) '(rs,ex) (WithResFunc res) ()
  WithResAllTrans ::
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
  NewResTrans ::
    ( MkRes m resPars
    , res ~ St (ResSt m resPars) name
    , zs ~ AppendToTupleList xs res
    , SplicC sp rs ex zs
    , Show resPars
    , KnownSymbol name
    , AppendToTuple (Variant xs) res
    , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
    , IsTypeUniqueList name xs 
    ) => Named name -> resPars -> STrans  q m sp xs '(rs,ex) (NewResFunc resPars name m) ()
  InvokeAllTrans ::
    ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
    , Show req
    , KnownSymbol name
    , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
    --, WhenStuck (ReqResult (NamedRequest req name) (VWrap xs NamedTuple)) (DelayError ('Text "No request supported detected"))
    , SplicC sp rs ex zs
    ) => Named name -> req -> STrans q m sp xs '(rs,ex) (InvokeAllFunc req name) ()
  ClearAllTrans ::
    ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
    , zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
    , KnownSymbol name
    , SplicC sp rs ex zs
    ) => Named name -> STrans q m sp xs '(rs,ex) (ClearAllFunc name) ()
  GetNextTrans ::
    ( ExtendStateTrans x
    , Transition m (ExtendedStateTrans x)
    , xs ~ (NextStates (ExtendedStateTrans x))
    , '(rs,ex) ~ Eval (GetNextFunc sp (x ': ys))
    , zs ~ Union rs ex
    , Liftable xs zs
    , Liftable ys zs
    , rs_ex ~ ListSplitterRes2 sp zs
    ) => STrans (ContT Bool) m sp (x ': ys) '(rs,ex) GetNextFunc ()
  GetNextAllTrans ::
    ( Transition m (TransWrap xs) 
    , SplicC sp rs ex zs
    , zs ~ NextStates (TransWrap xs)
    ) => STrans (ContT Bool) m sp xs '(rs,ex) GetNextAllFunc ()
  InvokeTrans ::
    ( TT x (TargetByName name x)
    , Request m req (TargetByName name x)
    , xs ~ (ReqResult req (TargetByName name x))
    , '(rs,ex) ~ Eval (InvokeFunc req name sp (x ': ys))
    , zs ~ Union rs ex
    , Liftable xs zs
    , Liftable ys zs
    , Show req
    , KnownSymbol name
    ) => Named name -> req -> STrans q m sp (x ': ys) '(rs,ex) (InvokeFunc req name) ()
  ClearAllVarTrans ::
    ( TermState m (ClrVar xs), MonadTrans q
    , rs_ex ~ Eval (ClearAllVarFunc sp xs)
    ) => STrans q m sp xs rs_ex ClearAllVarFunc ()
  ComposeTrans :: 
    ( ex_un ~ Union ex1 ex2
    , Liftable ex1 ex_un
    , Liftable ex2 ex_un
    , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
    , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
    , Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b 
    ) => STrans q m sp as '(rs1,ex1) f_a () -> STrans q m sp rs1 '(rs2,ex2) f_b b -> STrans q m sp as '(rs2,ex_un) (ComposeFunc f_a f_b) b
  ComposeDirTrans ::
    ( Eval (f_a sp as) ~ '(as, '[])  --assert 
    , Eval (f_b sp as) ~ '(rs2, ex2) --assert
    ) => STrans q m sp as '(as,'[]) f_a () -> STrans q m sp as '(rs2,ex2) f_b b -> STrans q m sp as '(rs2,ex2) f_b b
  BindTrans ::
    ( ex_un ~ Union ex1 ex2 
    , Liftable ex1 ex_un
    , Liftable ex2 ex_un
    , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
    , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
    , Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b
    ) => STrans q m sp as '(rs1,ex1) f_a a -> (a -> STrans q m sp rs1 '(rs2,ex2) f_b b) -> STrans q m sp as '(rs2,ex_un) (BindFunc f_a f_b) b
  BindDirTrans ::
    ( Eval (f_a sp as) ~ '(as, '[])  --assert 
    , Eval (f_b sp as) ~ '(rs2, ex2) --assert
    ) => STrans q m sp as '(as,'[]) f_a a -> (a -> STrans q m sp as '(rs2,ex2) f_b b) -> STrans q m sp as '(rs2,ex2) f_b b
  CaptureTrans ::
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
  EmbedTrans ::
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
  ReturnTrans :: a -> STrans q m sp xs '(xs, '[]) IDFunc a 
  ForeverTrans :: 
    ( Eval (f sp xs) ~ '(xs,ex)
    ) => STrans q m sp xs '(xs,ex) f () -> STrans q m sp xs '(('[]),ex) (ForeverFunc f) ()
  WhileTrans :: 
    ( Eval (f sp xs) ~ '(xs,ex)
    ) => STrans q m sp xs '(xs,ex) f Bool -> STrans q m sp xs '(xs,ex) f ()
  WhatNextTrans :: STrans q m sp xs '(xs, '[]) IDFunc (Proxy xs) 
  WhatSplitterTrans :: STrans q m sp xs '(xs, '[]) IDFunc (Proxy sp) 
  NoopTrans :: STrans q m sp xs '(xs, '[]) IDFunc () 
  ExtendWithFuncTrans :: 
    ( Liftable xs rs
    , '(rs, ex) ~ Eval (f sp xs)
    ) => STrans q m sp xs '(rs,ex) f () 
  ExtendWithFuncTrans_ :: 
    ( Liftable xs rs
    , '(rs, ex) ~ Eval (f sp xs)
    ) => STrans q m sp xs '(rs,'[]) f () 
  AlignTrans ::
    ( Liftable rs xs
    , '(rs, ex) ~ (Eval (f sp xs))
    ) => STrans q m sp xs '(rs,ex) f () -> STrans q m sp xs '(xs,ex) (AlignFunc f) ()  
  ExtendTo :: 
    ( Liftable xs rs
    ) => Proxy rs -> STrans q m sp xs '(rs,ex) (ConstFunc rs) () 
  ExtendForLoop :: 
    ( rs ~ First (TransformLoop sp xs f)
    , Liftable xs rs
    ) => STrans q m sp as '(bs,ex) f () -> STrans q m sp xs '(rs,'[]) (ExtendForLoopFunc f) () 
  Extend :: 
    ( Liftable xs rs
    ) => STrans q m sp xs '(rs,'[()]) (ConstFunc rs) () 
  GetTrans :: (x -> a) -> STrans q m sp '[x] '(('[x]), '[]) IDFunc a
  GetAllTrans :: (GetTypeByNameVar name x xs, x ~ St st name) => Named name -> (x -> a) -> STrans q m sp xs '(xs, '[]) IDFunc a
  OpAllTrans :: Monad m => m a -> STrans q m sp xs '(xs, '[]) IDFunc a
  OpResAllTrans :: (Monad m, GetTypeByNameVar name x xs) => Named name -> (x -> m a) -> STrans q m sp xs '(xs, '[]) IDFunc a
  IffTrans ::
    ( rs ~ Union rs1 xs
    , Liftable rs1 rs
    , Liftable xs rs
    , Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
    ) => Bool -> STrans q m sp xs '(rs1,ex) f1 ()  -> STrans q m sp xs '(rs, ex) (IffFunc f1) ()  
  IfElseTrans ::
    ( ex ~ Union ex1 ex2 
    , Liftable ex1 ex
    , Liftable ex2 ex
    , rs ~ Union rs1 rs2
    , Liftable rs1 rs
    , Liftable rs2 rs
    , Eval (f1 sp xs) ~ '(rs1, ex1)  --assert 
    , Eval (f2 sp xs) ~ '(rs2, ex2) --assert
    ) => Bool -> STrans q m sp xs '(rs1,ex1) f1 ()  -> STrans q m sp xs '(rs2,ex2) f2 () -> STrans q m sp xs '(rs, ex) (IfElseFunc f1 f2) ()  
  LiftIOTrans :: MonadIO m => IO a -> STrans q m sp xs '(xs, '[]) IDFunc a
  DictTrans ::
    ( TransDict q m dict keyName xs a
    , Eval (DictFunc name sp xs) ~ '(xs,'[])
    ) => dict -> Named keyName -> STrans q m sp xs '(xs, '[]) (DictFunc keyName) a
  AppWrapperTrans ::
    ( Eval (f sp xs) ~ rs_ex
    ) => STransApp q m sp xs rs_ex a -> STrans q m sp xs rs_ex f a 

splitV_ :: 
  ( ListSplitter sp ys
  , '(xs,ex) ~ ListSplitterRes2 sp ys 
  , VariantSplitter xs ex ys
  ) => sp -> V ys -> Either (V ex) (V xs, ())  
splitV_ sp v_ys = fmap (\v_xs->(v_xs,())) (splitV sp v_ys)

{-
applyTrans' :: 
  ( MonadTrans q
  , Monad (q m)
  , ListSplitterRes2 sp (Eval (sfunc sp xs)) ~ '(rs,ex)
  ) => STrans q m sp xs '(rs,ex) sfunc a -> sp -> MFlow q m xs ->  MFlowExA q m rs ex a
applyTrans' = applyTrans 
-}

{-
type ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b =
  ( ex_un ~ Union ex1 ex2
  , Liftable ex1 ex_un
  , Liftable ex2 ex_un
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , MonadTrans q
  , Monad (q m)
  )
-}

class -- (ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b) =>
  Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b where
    compose :: STrans q m sp as '(rs1,ex1) f_a () -> STrans q m sp rs1 '(rs2,ex2) f_b b -> sp -> MFlow q m as ->  MFlowExA q m rs2 ex_un b  

instance -- (ComposerC q m sp as rs1 '[] f_a rs2 ex2 f_b ex2 b) => 
  ( Eval (f_b sp rs1) ~ '(rs2, ex2)
  , Eval (f_a sp as) ~ '(rs1, '[])
  , MonadTrans q
  , Monad (q m)
  ) => Composer q m sp as rs1 '[] f_a rs2 ex2 f_b ex2 b where
    compose t1 t2 sp curSnap =  
      applyTrans t2 sp (fmap (\(Right (v_as,())) -> v_as) (applyTrans t1 sp curSnap))

instance 
  ( ex_un ~ Union ex1 ex2
  , Liftable (e ': ex1tail) ex_un
  , Liftable ex2 ex_un
  , Eval (f_b sp rs1) ~ '(rs2, ex2)
  , Eval (f_a sp as) ~ '(rs1, ex1)
  , MonadTrans q
  , Monad (q m)
  , ex1 ~ (e ': ex1tail)
  ) => Composer q m sp as rs1 (e ': ex1tail) f_a rs2 ex2 f_b ex_un b where
    compose t1 t2 sp curSnap = do 
      v_as1 <- applyTrans t1 sp curSnap
      case v_as1 of 
        Right (v_bs,()) ->
          let 
            liftLeft (Right r) = Right r 
            liftLeft (Left l) = Left $ liftVariant l
          in fmap liftLeft (applyTrans t2 sp (return v_bs))
        Left v_ex -> return $ Left $ liftVariant v_ex

--
class Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b where
    bind :: STrans q m sp as '(rs1,ex1) f_a a -> (a -> STrans q m sp rs1 '(rs2,ex2) f_b b) -> sp -> MFlow q m as ->  MFlowExA q m rs2 ex_un b  

instance 
  ( Eval (f_b sp rs1) ~ '(rs2, ex2)
  , Eval (f_a sp as) ~ '(rs1, '[])
  , MonadTrans q
  , Monad (q m)
  ) => Binder q m sp as rs1 '[] f_a a rs2 ex2 f_b ex2 b where
    bind t1 f_t2 sp curSnap =  
      applyTrans t1 sp curSnap >>= (\(Right (v_as,a)) -> applyTrans (f_t2 a) sp (return v_as))

          
instance 
  ( ex_un ~ Union ex1 ex2
  , Liftable (e ': ex1tail) ex_un
  , Liftable ex2 ex_un
  , Eval (f_b sp rs1) ~ '(rs2, ex2)
  , Eval (f_a sp as) ~ '(rs1, ex1)
  , MonadTrans q
  , Monad (q m)
  , ex1 ~ (e ': ex1tail)
  ) => Binder q m sp as rs1 (e ': ex1tail) f_a a rs2 ex2 f_b ex_un b where
    bind t1 f_t2 sp curSnap = do 
      v_as1 <- applyTrans t1 sp curSnap
      case v_as1 of 
        Right (v_bs,a) ->
          let 
            liftLeft (Right r) = Right r 
            liftLeft (Left l) = Left $ liftVariant l
          in fmap liftLeft (applyTrans (f_t2 a) sp (return v_bs))
        Left v_ex -> return $ Left $ liftVariant v_ex
      
applyTransSafely :: 
  ( MonadTrans q
  , Monad (q m)
  , Eval (sfunc sp xs) ~ '(rs,ex)
  ) => STrans q m sp xs '(rs,ex) sfunc a -> sp -> MFlow q m xs ->  MFlowExA q m rs ex a
applyTransSafely = applyTrans 

proxyFromFlow :: MFlow q m xs -> Proxy xs
proxyFromFlow _ = Proxy

applyTrans :: 
  ( MonadTrans q
  , Monad (q m)
  ) => STrans q m sp xs '(rs,ex) sfunc a -> sp -> MFlow q m xs ->  MFlowExA q m rs ex a
applyTrans (WithResTrans named resFact) sp curSnap = splitV_ sp <$> andRes named resFact curSnap
applyTrans (WithResAllTrans named resFact) sp curSnap = splitV_ sp <$> andResAll named resFact curSnap
applyTrans (NewResTrans named resPars) sp curSnap = splitV_ sp <$> andMkResAll named resPars curSnap
applyTrans (InvokeAllTrans named req) sp curSnap = splitV_ sp <$> andReqAll named req curSnap
applyTrans (ComposeTrans t1 t2) sp curSnap = compose t1 t2 sp curSnap 
applyTrans (ComposeDirTrans t1 t2) sp curSnap =  
  applyTrans t2 sp (fmap (\(Right (v_as,())) -> v_as) (applyTrans t1 sp curSnap))
applyTrans (BindTrans t1 f_t2) sp curSnap = 
  bind t1 f_t2 sp curSnap
applyTrans (BindDirTrans t1 f_t2) sp curSnap = 
  applyTrans t1 sp curSnap >>= (\(Right (v_as,a)) -> applyTrans (f_t2 a) sp (return v_as))
applyTrans (ReturnTrans a) sp curSnap = fmap (\v_xs -> Right (v_xs,a)) curSnap
applyTrans (LiftIOTrans io_a) sp curSnap = do 
  v_xs <- curSnap
  a <- lift $ liftIO io_a 
  return $ Right (v_xs, a)
applyTrans (CaptureTrans sp1 t) sp curSnap = do 
  v_xs <- curSnap
  case splitV sp1 v_xs of
    Right v_xs1 -> do
      fmap (fmap (\(v_xs2,())-> (liftVariant v_xs2,()))) (applyTrans t sp (return v_xs1))
    Left v_ex -> return $Right (liftVariant v_ex,())
applyTrans (EmbedTrans sp1 t) sp curSnap = 
  let 
    merge3 ::
      ( zs ~ Union xs (Union ex1 ex2)
      , Liftable xs zs
      , Liftable ex1 zs
      , Liftable ex2 zs
      ) => Either (Either (V ex2) (V ex1)) (V xs) -> V zs
    merge3 (Left (Left v_ex2)) = liftVariant v_ex2  
    merge3 (Left (Right v_ex1)) = liftVariant v_ex1 
    merge3 (Right v_xs) = liftVariant v_xs
  in do 
    v_xs <- curSnap 
    v_3 <- case splitV sp1 v_xs of 
      Right v_x_sub -> do 
        ei_xs1_ex1 <- applyTrans t (sp :&& sp1) (return v_x_sub)
        case ei_xs1_ex1 of 
          Right (v_xs1,()) -> return $ Right v_xs1 --liftVariant v_xs1
          Left v_ex1 -> return $ (Left (Left v_ex1)) -- liftVariant v_ex1
      Left v_ex -> return $ (Left (Right v_ex)) -- (liftVariant v_ex)
    let v_zs = merge3 v_3  
    return (splitV_ sp v_zs)
applyTrans (ForeverTrans t) sp curSnap = do 
  ei_xs_ex <- applyTrans t sp curSnap 
  case ei_xs_ex of 
    Right (v_xs, ()) -> applyTrans (ForeverTrans t) sp (return v_xs)
    Left v_ex -> return $ Left v_ex
applyTrans (WhileTrans t) sp curSnap= do 
  ei_xs_ex <- applyTrans t sp curSnap 
  case ei_xs_ex of 
    Right (v_xs, True) -> applyTrans (WhileTrans t) sp (return v_xs)
    Right (v_xs, False) -> return $ Right (v_xs,())
    Left v_ex -> return $ Left v_ex
applyTrans WhatNextTrans sp curSnap = fmap (\v->Right (v,Proxy)) curSnap  
applyTrans WhatSplitterTrans sp curSnap = fmap (\v->Right (v,Proxy)) curSnap
applyTrans NoopTrans sp curSnap = fmap (\v->Right (v,())) curSnap
applyTrans GetNextAllTrans sp curSnap = splitV_ sp <$> andNextAll curSnap      
applyTrans (ClearAllTrans named) sp curSnap = splitV_ sp <$>  andClearAll named curSnap
applyTrans ClearAllVarTrans sp curSnap = Right <$>  andClearAllVar curSnap
applyTrans ExtendWithFuncTrans sp curSnap = fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans ExtendWithFuncTrans_ sp curSnap = fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans (AlignTrans t) sp curSnap = do
  v_rs_ex  <- applyTrans t sp curSnap 
  case v_rs_ex of 
    Right (v_xs,()) -> return $ Right (liftVariant v_xs, ())
    Left v_ex -> return $ Left v_ex
applyTrans (ExtendTo _) sp curSnap = fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans (ExtendForLoop _) sp curSnap = fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans Extend sp curSnap = fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans (GetTrans f) sp curSnap = fmap (\v-> Right (v,(f (variantToValue v)))) curSnap
applyTrans (GetAllTrans named f) sp curSnap = fmap (\v-> Right (v,(f (getTypeByNameVar named v)))) curSnap
applyTrans (OpResAllTrans named f) sp curSnap = do 
  v_xs <- curSnap
  op_res <- lift $ f (getTypeByNameVar named v_xs)
  return $ Right (v_xs, op_res)
applyTrans (OpAllTrans m_a) sp curSnap = do 
  v_xs <- curSnap
  a <- lift m_a
  return $ Right (v_xs,a)
applyTrans (IfElseTrans fl t1 t2) sp curSnap = 
  if fl
    then 
      fmap liftRes (applyTrans t1 sp curSnap)
    else 
      fmap liftRes (applyTrans t2 sp curSnap)
applyTrans (IffTrans fl t1) sp curSnap = 
  if fl
    then
      fmap liftRightRes (applyTrans t1 sp curSnap)
    else 
      fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans (DictTrans dict named) sp curSnap = applyTransApp (getTransFromDict dict named) sp curSnap
applyTrans (AppWrapperTrans app) sp curSnap = applyTransApp app sp curSnap
       
liftRes :: (Liftable rs1 rs, Liftable ex1 ex) => Either (V ex1) (V rs1,()) -> Either (V ex) (V rs,())
liftRes (Right (v_rs1, ())) = Right $ (liftVariant v_rs1, ())        
liftRes (Left v_ex1) = Left $ liftVariant v_ex1

liftRightRes :: (Liftable rs1 rs) => Either (V ex) (V rs1,()) -> Either (V ex) (V rs,())
liftRightRes (Right (v_rs1, ())) = Right $ (liftVariant v_rs1, ())        
liftRightRes (Left v_ex) = Left v_ex


type family TransformLoop sp (xs :: [*]) (f :: * -> [*] -> Exp ([*],[*])) :: ([*], [*]) where
  TransformLoop sp xs f = TransformLoop' sp (Eval (f sp xs)) '(xs,'[]) f    

type family TransformLoop' sp (nextXsEx :: ([*], [*])) (totalXsEx :: ([*], [*])) (f :: * -> [*] -> Exp ([*],[*])) ::  ([*], [*]) where
  TransformLoop' sp '(('[]), nextEx)  '(totalXs, totalEx) f = '(totalXs, Union totalEx nextEx)    
  TransformLoop' sp '(nextXs, nextEx) '(totalXs, totalEx) f = TransformLoop'' sp '(nextXs, nextEx)  (IsSublist totalXs nextXs) '(totalXs, totalEx) f     
  
type family TransformLoop'' sp (nextXs :: ([*],[*])) (isSublist :: Bool) (totalXs :: ([*],[*])) (f :: * -> [*] -> Exp ([*],[*])) :: ([*], [*]) where
  TransformLoop'' sp nextXs True sumXs f = sumXs    
  TransformLoop'' sp '(nextXs, nextEx) False '(totalXs, totalEx) f = TransformLoop' sp (Eval (f sp nextXs)) '(Union totalXs nextXs, Union totalEx nextEx)  f    

data TransformLoopFunc ::  (* -> [*] -> Exp ([*],[*])) -> * -> [*] -> Exp ([*],[*])
type instance Eval (TransformLoopFunc f sp xs) = TransformLoop sp xs f


transRes :: STrans q m sp xs rs_ex sfunc a -> Proxy (Eval (sfunc sp xs))
transRes _ = Proxy

transFunc :: forall q m sp xs rs_ex sfunc a. STrans q m sp xs rs_ex sfunc a -> Proxy sfunc
transFunc _ = Proxy

execTrans' :: 
 ( MonadTrans q
 , Monad (q m)
 --, Eval (sfunc NoSplitter '[()]) ~ '(rs,ex)
 ) => STrans q m NoSplitter '[()] '(rs,ex) sfunc a -> MFlowExA q m rs ex a
execTrans' t = applyTrans t NoSplitter (return (variantFromValue ()))

execTrans_ :: 
  ( MonadTrans q
  , Monad (q m)
  -- , Eval (sfunc NoSplitter '[()]) ~ '(rs,'[])
  ) => STrans q m NoSplitter '[()] '(rs,'[]) sfunc () -> MFlow q m rs 
execTrans_ t = do 
  ei <- applyTrans t NoSplitter (return (variantFromValue ()))
  case ei of
    Right (v_rs,()) ->  return v_rs
    Left _ -> undefined -- cannot happen as ex ~ '[]

type ExecutableFunc sfunc = Eval (sfunc NoSplitter '[()]) ~ '(('[()]),'[])    
type ExcecutableTrans q m sfunc = STrans q m NoSplitter '[()] '(('[()]),'[]) sfunc ()
type ExcecutableApp q m = STransApp q m NoSplitter '[()] '(('[()]),'[]) ()

type AsyncTrans m sp xs rs_ex func a = STrans (ContT Bool) m sp xs rs_ex a  
type SyncTrans m sp xs rs_ex func a = STrans IdentityT m sp xs rs_ex a  

type AsyncTransApp m rs_ex func = STrans (ContT Bool) m NoSplitter '[()] rs_ex func ()  
type SyncTransApp m rs_ex func = STrans IdentityT m NoSplitter '[()] rs_ex func ()  

execTrans ::  
  ( MonadTrans q
  --, ExecutableFunc sfunc
  , Monad (q m)
  ) => ExcecutableTrans q m sfunc -> q m () 
execTrans t = fmap variantToValue (execTrans_ t)  
    

type EvalTransFunc m func = Eval (func m NoSplitter '[()])

data STransApp q (m :: * -> *) (sp :: *) (xs :: [*]) (rs_ex :: ([*],[*]))  (a :: *) where
  MkApp :: STrans q m sp xs rs_ex func a -> STransApp  q m sp xs rs_ex a 

--mkIDTrans :: STransApp q m sp xs '(xs, '[]) a -> STrans q m sp xs '(xs, '[]) IDFunc a
--mkIDTrans (MkApp t) = t


applyTransApp :: 
  ( MonadTrans q
  , Monad (q m)
  ) => STransApp q m sp xs '(rs,ex) a -> sp -> MFlow q m xs ->  MFlowExA q m rs ex a
applyTransApp (MkApp trans) = applyTrans trans  
  
execApp ::  (MonadTrans q, Monad (q m)) => ExcecutableApp q m  -> q m () 
execApp (MkApp t) = execTrans t  
    
----
type family Has (st :: *) (xs :: [*]) where
  Has (St st name) xs = GetTypeByNameVar name (St st name) xs

type family HasWithName (st :: *) (name :: Symbol) (xs :: [*]) where
  HasWithName (St st name) name1 xs = (GetTypeByNameVar name (St st name) xs, (St st name) ~ (St st name1))
  