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
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Beseder.Base.Internal.STransIx where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Identity (IdentityT, runIdentityT)
import           Control.Monad.Trans (MonadTrans)
import           Haskus.Utils.Flow            hiding (forever)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
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
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.ResourceList
import           Control.Arrow (Kleisli (..))
import           Beseder.Base.Internal.STransMonad hiding (return, (>>), (>>=))
import           Data.Coerce
import qualified Prelude as SafeUndef (undefined) 

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

type family NameOfRes res :: Symbol where
  NameOfRes (St st resName) = resName

type family TypeOfRes res  where
  TypeOfRes (St st resName) = st

type family Has (st :: *) (xs :: [*]) where
  Has (St st name) xs = GetTypeByNameVar name (St st name) xs

type family HasWithName (st :: *) (name :: Symbol) (xs :: [*]) where
  HasWithName (St st name) name1 xs = (GetTypeByNameVar name (St st name) xs, (St st name) ~ (St st name1))
    
    
splitV_ :: 
  ( ListSplitter sp ys
  , '(xs,ex) ~ ListSplitterRes2 sp ys 
  , VariantSplitter xs ex ys
  ) => sp -> V ys -> Either (V ex) (V xs, ())  
splitV_ sp v_ys = fmap (\v_xs->(v_xs,())) (splitV sp v_ys)

{-
class 
  ('(rs_n,ex_n) ~ Eval (ReplicateFunc n sfunc sp xs)) =>
    ReplcateTrans (n :: NatOne) q (m :: * -> *) (sp :: *) xs rs_n ex_n (sfunc :: * -> [*] -> ([*],[*]) -> *) where
      replicateTrans :: Proxy n -> STransF q m sp sfunc () -> STrans q m sp xs rs_n ex_n (ReplicateFunc n sfunc) ()  

instance (Eval (sfunc sp xs) ~  '(rs, ex), '(rs,ex) ~ Eval (ReplicateFunc One sfunc sp xs)) =>
  -- (forall xs rs ex. Eval (sfunc sp xs) ~  '(rs, ex), forall xs rs ex. '(rs,ex) ~ Eval (ReplicateFunc One sfunc sp xs)) => 
  ReplcateTrans One q m sp xs rs ex sfunc where
    replicateTrans _px (MkF (STrans t)) = STrans (\sp v_xs -> t sp v_xs)
-}

{-  
instance (ReplicateTrans n q m sp xs rs ex rs_n ex_n sfunc) => ReplcateTrans (Succ n) q m sp xs rs ex rs_n1 ex_n1 sfunc where
  replicateTrans _px strans@(STrans t) = do
    replicateTrans (Proxy@) 
-}

--  
newtype STrans (q :: (* -> *) -> * -> *) (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*]) (sfunc :: * -> [*] -> Exp ([*],[*])) (a :: *) =
        STrans {runTrans :: sp -> V xs -> q m (Either (V ex) (V rs,a))}

data STransApp (q :: (* -> *) -> * -> *) (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*]) (a :: *) where
  MkApp :: STrans q m sp xs rs ex func a -> STransApp  q m sp xs rs ex a 

returnT :: Monad (q m) => a -> STrans q m sp xs xs '[] (ReturnFunc a) a
returnT a = STrans (\_sp v_xs -> return (Right (v_xs,a)))

bindT :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
  --, '(rs1,ex1) ~ Eval (f1 sp xs)
  --, '(rs2,ex2) ~ Eval (f2 sp rs1)  
  ) => STrans q m sp xs rs1 ex1 f1 a -> (a -> STrans q m sp rs1 rs2 ex2 f2 b) -> STrans q m sp xs rs2 (Concat ex1 ex2) (BindFunc f1 f2) b
bindT (STrans t1) f = 
    STrans $ (\sp v_xs -> 
      do
        ei_rs1a_ex1 <- t1 sp v_xs
        ei_res <- case ei_rs1a_ex1 of 
          Right (v_rs1,a) ->
            let 
              (STrans t2) = f a
              -- mapResult :: Either (V ex2) (V rs2, b) -> Either (Either (V ex1) (V ex2)) (V rs2, b)
              mapResult (Right r) = Right r 
              mapResult (Left ex2) = Left (Right ex2) -- :: Either (V ex1) (V ex2))
            in fmap mapResult (t2 sp v_rs1)
          Left v_ex1 -> return $ Left (Left v_ex1) --  :: Either (V ex1) (V ex2))
        return (  
          case ei_res of
            Right v_rs2_b -> Right v_rs2_b
            Left ei_ex1_ex2 -> Left $ concatEither ei_ex1_ex2))    

class Composable q m sp xs rs1 ex1 rs2 ex2 f1 f2 a where
  compose :: STrans q m sp xs rs1 ex1 f1 () -> STrans q m sp rs1 rs2 ex2 f2 a -> STrans q m sp xs rs2 (Concat ex1 ex2) (ComposeFunc f1 f2) a            

composeT :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
  -- , '(rs1,ex1) ~ Eval (f1 sp xs)
  -- , '(rs2,ex2) ~ Eval (f2 sp rs1)  
  ) => STrans q m sp xs rs1 ex1 f1 () -> STrans q m sp rs1 rs2 ex2 f2 b -> STrans q m sp xs rs2 (Concat ex1 ex2) (ComposeFunc f1 f2) b
composeT (STrans t1) (STrans t2) = 
    STrans $ (\sp v_xs -> 
      do
        ei_rs1a_ex1 <- t1 sp v_xs
        ei_res <- case ei_rs1a_ex1 of 
          Right (v_rs1,()) ->
            let 
              -- mapResult :: Either (V ex2) (V rs2, b) -> Either (Either (V ex1) (V ex2)) (V rs2, b)
              mapResult (Right r) = Right r 
              mapResult (Left ex2) = Left (Right ex2) -- :: Either (V ex1) (V ex2))
            in fmap mapResult (t2 sp v_rs1)
          Left v_ex1 -> return $ Left (Left v_ex1) --  :: Either (V ex1) (V ex2))
        return (  
          case ei_res of
            Right v_rs2_b -> Right v_rs2_b
            Left ei_ex1_ex2 -> Left $ concatEither ei_ex1_ex2))    

{-
scopeT ::              
  ( Monad (q m)
  , KnownNat (Length ex1)
  --, rs1 ~ First (Eval (f1 sp xs))
  ) => STrans q m sp xs rs1 ex1 f1 () -> STrans q m sp rs1 rs2 ex2 (Eval (fd xs rs1)) () -> STrans q m sp xs rs2 (Concat ex1 ex2) (ScopeFunc f1 fd) ()
scopeT t1 t2 = unsafeRefunc $ composeT t1 t2 
-}

scopeT ::              
  ( Monad (q m)
  , KnownNat (Length rs1)
  , rs1_ex1 ~ Concat rs1 ex1
  --, rs1 ~ First (Eval (f1 sp xs))
  -- , '(rs2, ex2) ~ Eval (ScopeFunc f1 fd sp xs) -- just an assertion
  ) => STrans q m sp xs rs1 ex1 f1 () -> STrans q m sp rs1_ex1 rs2 ex2 (Eval (fd xs rs1_ex1)) () -> STrans q m sp xs rs2 ex2 (ScopeFunc f1 fd) ()
scopeT (STrans t1) (STrans t2) = -- unsafeRefunc $ composeT t1 t2 
    STrans $ (\sp v_xs -> 
      do
        ei_rs1_ex1 <- t1 sp v_xs
        let v_rs1_ex1 = concatRightLeft ei_rs1_ex1
        t2 sp v_rs1_ex1)

funcT ::              
  ( Monad (q m)
  ) => STrans q m sp xs rs ex (Eval (ff xs)) () -> STrans q m sp xs rs ex (FuncFunc ff) ()
funcT t = unsafeRefunc $ t 

newRes ::
  ( MkRes m resPars
  , res ~ St (ResSt m resPars) name
  , zs ~ AppendToTupleList xs res
  , SplicC sp rs ex zs
  , Monad (q m)
  , MonadTrans q
  , KnownSymbol name
  , AppendToTuple (Variant xs) res
  , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
  , IsTypeUniqueList name xs 
  ) => Named name -> resPars -> STrans  q m sp xs rs ex (NewResFunc resPars name m) ()
newRes named resPars = STrans (\sp v_xs -> splitV_ sp <$> andMkResAll named resPars (return v_xs))  

invoke ::
  ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
  , Show req
  , KnownSymbol name
  , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  , Monad (q m)
  , MonadTrans q
  --, WhenStuck (ReqResult (NamedRequest req name) (VWrap xs NamedTuple)) (DelayError ('Text "No request supported detected"))
  , SplicC sp rs ex zs
  ) => Named name -> req -> STrans q m sp xs rs ex (InvokeAllFunc req name) ()
invoke named req = STrans (\sp v_xs -> splitV_ sp <$> andReqAll named req (return v_xs))  

renameRes :: 
  ( RenameResource (V xs) resName newName
  , V zs ~ RenameResourceRes (V xs) resName newName
  , Monad (q m)
  , MonadTrans q
  , SplicC sp rs ex zs
  ) => Named resName -> Named newName -> STrans q m sp xs rs ex (RenameResFunc resName newName) () 
renameRes resName newName = STrans (\sp v_xs -> return $ splitV_ sp (renameResource v_xs resName newName))  

clear ::
  ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , Monad (q m)
  , MonadTrans q
  , KnownSymbol name
  , SplicC sp rs ex zs
  ) => Named name -> STrans q m sp xs rs ex (ClearAllFunc name) ()
clear named = STrans (\sp v_xs -> splitV_ sp <$> andClearAll named (return v_xs))  

nextEv' ::
  ( Transition m (TransWrap xs) 
  , SplicC sp rs ex zs
  , zs ~ NextStates (TransWrap xs)
  ) => STrans (ContT Bool) m sp xs rs ex GetNextAllFunc ()
nextEv' = STrans (\sp v_xs -> splitV_ sp <$> andNextAll (return v_xs))

embed ::
  ( sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , Monad (q m)
  , MonadTrans q
  ) => sp1 -> STrans q m (sp :&& sp1) xs_sub rs_sub ex f_sub () -> STrans q m sp xs rs ex1 (EmbedFunc sp1 f_sub) ()
embed sp1 (STrans t) = 
  STrans 
    (\sp v_xs ->
        let 
          merge3 ::
            ( zs ~ Union xs (Union ex1 ex2)
            , Liftable xs zs
            , Liftable ex1 zs
            , Liftable ex2 zs
            ) => Either (Either (V ex2) (V ex1)) (V xs) -> V zs
          merge3 (Left (Left v_ex2)) = liftVariant v_ex2  
          merge3 (Left (Right v_ex1)) = liftVariant v_ex1 
          merge3 (Right v_xs') = liftVariant v_xs'
        in do 
          v_3 <- case splitV sp1 v_xs of 
            Right v_x_sub -> do 
              ei_xs1_ex1 <- t (sp :&& sp1) v_x_sub
              case ei_xs1_ex1 of 
                Right (v_xs1,()) -> return $ Right v_xs1 
                Left v_ex1 -> return $ (Left (Left v_ex1)) 
            Left v_ex -> return $ (Left (Right v_ex)) 
          let v_zs = merge3 v_3  
          return (splitV_ sp v_zs))

try :: forall sp1 sp sp2 xs_sub ex_sub ex zs rs_sub rs q m ex1 xs f_sub.
  ( sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  ) => STrans q m (sp :&& sp1) xs_sub rs_sub ex f_sub () -> STrans q m sp xs rs ex1 (EmbedFunc sp1 f_sub) ()
try t = embed (getInstance @sp1) t

reach :: forall sp1_not sp1 sp sp2 xs_sub ex_sub ex zs rs_sub rs q m ex1 xs f_sub.
  ( sp1 ~ Not sp1_not
  , sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  ) => STrans q m (sp :&& sp1) xs_sub rs_sub ex f_sub () -> STrans q m sp xs rs ex1 (EmbedFunc sp1 f_sub) ()
reach t = embed (getInstance @(Not sp1_not)) t

capture ::
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , Monad (q m)
  , MonadTrans q
  ) => sp1 -> STrans q m sp xs_sub rs_sub ex f_sub () -> STrans q m sp xs rs1 ex (CaptureFunc sp1 f_sub) ()
capture sp1 (STrans t) =
  STrans 
    (\sp v_xs ->
      case splitV sp1 v_xs of
        Right v_xs1 -> do
          fmap (fmap (\(v_xs2,())-> (liftVariant v_xs2,()))) (t sp v_xs1)
        Left v_ex -> return $ Right (liftVariant v_ex,()))
    

on :: forall sp1 xs xs_sub q m ex_sub ex rs1 f_sub rs_sub sp.
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  ) => STrans q m sp xs_sub rs_sub ex f_sub () -> STrans q m sp xs rs1 ex (CaptureFunc sp1 f_sub) ()
on t = capture (getInstance @sp1) t

captureOrElse ::
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs ~ Union rs_sub1 rs_sub2  
  , Liftable rs_sub1 rs
  , Liftable rs_sub2 rs
  , ex ~ Union ex1 ex2  
  , Liftable ex1 ex
  , Liftable ex2 ex
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  ) => sp1 -> STrans q m sp xs_sub rs_sub1 ex1 f_sub1 () -> STrans q m sp ex_sub rs_sub2 ex2 f_sub2 () -> STrans q m sp xs rs ex (CaptureOrElseFunc sp1 f_sub1 f_sub2) ()
captureOrElse sp1 (STrans t1) (STrans t2) = do
  STrans 
    (\sp v_xs -> 
        case splitV sp1 v_xs of
          Right v_xs1 -> 
            fmap liftRes (t1 sp v_xs1)
          Left v_ex -> 
            fmap liftRes (t2 sp v_ex))

onOrElse :: forall sp1 sp xs xs_sub ex_sub rs rs_sub1 rs_sub2 ex1 ex2 ex q m f_sub1 f_sub2. 
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs ~ Union rs_sub1 rs_sub2  
  , Liftable rs_sub1 rs
  , Liftable rs_sub2 rs
  , ex ~ Union ex1 ex2  
  , Liftable ex1 ex
  , Liftable ex2 ex
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  ) => STrans q m sp xs_sub rs_sub1 ex1 f_sub1 () -> STrans q m sp ex_sub rs_sub2 ex2 f_sub2 () -> STrans q m sp xs rs ex (CaptureOrElseFunc sp1 f_sub1 f_sub2) ()
onOrElse t1 t2 = captureOrElse (getInstance @sp1) t1 t2
                      
liftRes :: (Liftable rs1 rs, Liftable ex1 ex) => Either (V ex1) (V rs1,()) -> Either (V ex) (V rs,())
liftRes (Right (v_rs1, ())) = Right $ (liftVariant v_rs1, ())        
liftRes (Left v_ex1) = Left $ liftVariant v_ex1

block :: 
  ( Monad (q m)
  , MonadTrans q
  ) => STrans q m sp xs ys ex f () -> STrans q m sp xs ys ex (BlockFunc n f) ()
block = coerce 

assert :: forall sp1 q m sp xs. 
  ( Monad (q m)
  , MonadTrans q
  , '(xs, '[]) ~ ListSplitterRes2 sp1 xs 
  ) => STrans q m sp xs xs '[] (AssertFunc sp1) ()
assert = STrans (\_sp v_xs -> return $ Right (v_xs, ()))

forever :: 
  ( Monad (q m)
  , MonadTrans q
  -- , Eval (f sp xs) ~ '(xs,ex)
  ) => STrans q m sp xs xs ex f () -> STrans q m sp xs ('[]) ex (ForeverFunc f) ()
forever (STrans t) =
  STrans 
    (\sp v_xs_init -> 
      let 
        go v_xs = do
          ei_xs_ex <- t sp v_xs
          case ei_xs_ex of 
            Right (v_xs1, ()) -> go v_xs1
            Left v_ex -> return $ Left v_ex
      in 
        go v_xs_init)

while :: 
  ( Monad (q m)
  , MonadTrans q
  -- , Eval (f sp xs) ~ '(xs,ex)
  ) => STrans q m sp xs xs ex f Bool -> STrans q m sp xs xs ex (WhileFunc f) ()
while (STrans t) =
  STrans 
    (\sp v_xs_init -> 
      let 
        go v_xs = do
          ei_xs_ex <- t sp v_xs
          case ei_xs_ex of 
            Right (v_xs1, True) -> go v_xs1
            Right (v_xs1, False) -> return $ Right (v_xs1,())
            Left v_ex -> return $ Left v_ex
      in 
        go v_xs_init)


nextEv :: 
  ( sp1 ~ Dynamics
  , SplicC sp1 xs_sub ex_sub xs
  , Transition m (TransWrap xs_sub) 
  , zs ~ NextStates (TransWrap xs_sub)
  , SplicC sp rs ex zs
  , rs1 ~ (Union rs ex_sub)
  , Liftable rs rs1
  , Liftable ex_sub rs1
  ) => STrans (ContT Bool) m sp xs rs1 ex (CaptureFunc Dynamics GetNextAllFunc) ()
nextEv = on @Dynamics nextEv' 


gets :: -- forall x name xs st sp q m a.
  ( GetTypeByNameVar name x xs
  , x ~ St st name
  , Monad (q m)
  ) => Named name -> (x -> a) -> STrans q m sp xs xs ('[]) (GetFunc name x) a 
gets named f = STrans (\_sp v_xs -> return $ Right (v_xs , (f (getTypeByNameVar named v_xs)))) 

prop :: -- forall x name xs st sp q m a.
  ( GetPropVarByName xs name propKey
  , Monad (q m)
  ) => Named name -> Proxy propKey -> STrans q m sp xs xs ('[]) (GetPropFunc name propKey) (PropType propKey)
prop named pxKey = STrans (\_sp v_xs -> return $ Right (v_xs , getPropVarByName v_xs named pxKey)) 

op :: 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  ) => m a -> STrans q m sp xs xs  ('[]) (OpFunc a) a
op ma = 
  STrans (\_sp v_xs -> lift ma >>= (\a -> return $ Right (v_xs,a))) 

opRes :: 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , GetTypeByNameVar name x xs
  ) => Named name -> (x -> m a) -> STrans q m sp xs xs ('[]) (OpResFunc name x) a
opRes named f =
  STrans 
    (\_sp v_xs -> do
      op_res <- lift $ f (getTypeByNameVar named v_xs)
      return $ Right (v_xs, op_res))

opInter :: 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , cx xs m
  --) => Proxy cx -> (forall xs. cx xs m => V xs -> m a) -> STrans q m sp xs xs ('[]) (OpInterFunc cx) a
  ) => (cx xs m => V xs -> m a) -> STrans q m sp xs xs ('[]) (OpInterFunc cx) a
  --) => Proxy cx -> (cx xs m => V xs -> m a) -> STrans q m sp xs xs ('[]) (OpInterFunc cx) a
opInter  f =
  STrans 
    (\_sp v_xs -> do
      res <- lift $ f v_xs
      return $ Right (v_xs, res))

iff ::
  ( rs ~ Union rs1 xs
  , Liftable rs1 rs
  , Liftable xs rs
  , Monad (q m)
  -- , Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
  ) => Bool -> STrans q m sp xs rs1 ex f1 ()  -> STrans q m sp xs rs ex (IffFunc f1) () 
iff fl (STrans t) =
  STrans 
    (\sp v_xs ->
      if fl
        then do
          ei <- t sp v_xs
          case ei of
            Left v_ex -> return $ Left v_ex
            Right (v_rs1,()) -> return (Right (liftVariant v_rs1,()))
        else return (Right (liftVariant v_xs,())))

ifElse ::
  ( ex ~ Union ex1 ex2 
  , Liftable ex1 ex
  , Liftable ex2 ex
  , rs ~ Union rs1 rs2
  , Liftable rs1 rs
  , Liftable rs2 rs
  , Monad (q m)
  --, Eval (f1 sp xs) ~ '(rs1, ex1)  --assert 
  --, Eval (f2 sp xs) ~ '(rs2, ex2) --assert
  ) => Bool -> STrans q m sp xs rs1 ex1 f1 ()  -> STrans q m sp xs rs2 ex2 f2 () -> STrans q m sp xs rs ex (IfElseFunc f1 f2) ()  
ifElse fl (STrans t1) (STrans t2) =    
  STrans 
    (\sp v_xs ->
      if fl
        then do
          ei <- t1 sp v_xs
          case ei of
            Left v_ex1 -> return $ Left (liftVariant v_ex1)
            Right (v_rs1,()) -> return (Right (liftVariant v_rs1,()))
        else do 
          ei <- t2 sp v_xs
          case ei of
            Left v_ex2 -> return $ Left (liftVariant v_ex2)
            Right (v_rs2,()) -> return (Right (liftVariant v_rs2,())))
ifJust ::
  ( rs ~ Union rs1 xs
  , Liftable rs1 rs
  , Liftable xs rs
  , Monad (q m)
  --, Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
  ) => (Maybe b) -> (b -> STrans q m sp xs rs1 ex f1 ())  -> STrans q m sp xs rs ex (IfJustFunc f1) ()  
ifJust bMaybe f = 
  STrans 
    (\sp v_xs -> case bMaybe of
      Just b -> do
        ei <- runTrans (f b) sp v_xs
        case ei of 
          Left v_ex1 -> return $ Left v_ex1
          Right (v_rs1,()) -> return (Right (liftVariant v_rs1,()))
      Nothing  -> return (Right (liftVariant v_xs,())))     

liftIO :: 
  ( MonadIO m
  , MonadTrans q
  , Monad (q m)
  ) => IO a -> STrans q m sp xs xs ('[]) LiftIOFunc a
liftIO ioa = refunc $ op (Protolude.liftIO ioa)

whatNext :: (s ~ Proxy xs, Monad (q m)) => STrans q m sp xs xs ('[]) (WhatNextFunc s) (Proxy xs)
whatNext = STrans (\_sp v_xs -> return $ Right (v_xs, proxyOfVar v_xs))

whatNames :: (pxNames ~ Proxy (GetAllNames xs), Monad (q m)) => STrans q m sp xs xs ('[]) (WhatNamesFunc names) pxNames
whatNames = STrans (\_sp v_xs -> return $ Right (v_xs, proxyOfNames v_xs))

whatSteps :: Monad (q m) => STrans q m sp xs xs ('[]) WhatStepsFunc (Proxy (TotalSteps sp xs GetNextAllFunc))
whatSteps = STrans (\_sp v_xs -> return $ Right (v_xs, Proxy))

noop :: Monad (q m) => STrans q m sp xs xs ('[]) NoopFunc ()
noop = STrans (\_sp v_xs -> return $ Right (v_xs, ()))

label :: Monad (q m) => Named label -> STrans q m sp xs xs ('[]) (LabelFunc label) ()
label _ = STrans (\_sp v_xs -> return $ Right (v_xs, ()))

--appT :: (Eval (f sp xs) ~ '(rs,ex)) => STransApp q m sp xs rs ex () -> STrans q m sp xs rs ex f ()
--appT (MkApp t) = coerce t

query :: Monad (q m) => Proxy xs -> STrans q m sp xs xs ('[]) QueryStateFunc ()
query _ = refunc noop

--needed?
order ::  
  ( Monad (q m)
  , v_ys ~ Variant ys
  , Variant ys ~ OrderByNameRes names (V xs)
  , SplicC sp rs ex ys
  , OrderByName names (V xs) 
  ) => Proxy names -> STrans q m sp xs rs ex (OrderFunc names) ()
order names = STrans (\sp v_xs -> return $ splitV_ sp (orderByName names v_xs)) 

newState :: 
  ( Eval (f sp xs) ~ '(xs1,ex)
  , xs2 ~ FilterList xs xs1
  , xs3 ~ FilterList xs2 xs1
  , VariantSplitter xs2 xs3 xs1
  , Liftable xs3 xs
  , Monad (q m)
  ) => STrans q m sp xs xs1 ex f () -> STrans q m sp xs xs2 ex (GetNewStateFunc f) ()
newState (STrans t) =
  STrans 
    (\sp v_xs_init ->  
      let 
        go v_xs = do
          ei_xs_ex <- t sp v_xs 
          case ei_xs_ex of 
            Right (v_xs1, ()) -> do
              let px_xs = proxyOfVar v_xs
                  px_xs2 = proxyOfFilter v_xs1 px_xs
              case splitVar1' v_xs1 px_xs2 of
                Left v_xs2 -> return (Right (v_xs2,()))
                Right v_xs3 -> go (liftVariant v_xs3)   
            Left v_ex -> return $ Left v_ex
      in go v_xs_init)       

termAndClearAllResources ::
  ( TermState m (ClrVar xs), MonadTrans q
  , '(rs, ex) ~ Eval (ClearAllVarFunc sp xs)
  , Monad (q m)
  ) => STrans q m sp xs rs ex ClearAllVarFunc ()
termAndClearAllResources = 
  STrans 
    (\_sp v_xs -> Right <$> andClearAllVar (return v_xs))

extendForHandlerLoop:: 
  ( rs ~ First (TransformLoop sp xs f)
  , Liftable xs rs
  , Monad (q m)
  ) => STrans q m sp as bs ex f () -> STrans q m sp xs rs ('[]) (ExtendForLoopFunc f) () 
extendForHandlerLoop _strans =
  STrans (\_sp v_xs -> return $ Right (liftVariant v_xs,()))

alignWithHandler ::
  ( Liftable rs loopRes
  , '(rs, ex) ~ (Eval (f sp loopRes))
  , Monad (q m)
  ) => STrans q m sp loopRes rs ex f () -> STrans q m sp loopRes loopRes ex (AlignFunc f) ()
alignWithHandler (STrans t) =     
  STrans 
    (\sp v_xs -> do
      v_rs_ex  <- t sp v_xs
      case v_rs_ex of 
        Right (v_xs1,()) -> return $ Right (liftVariant v_xs1, ())
        Left v_ex -> return $ Left v_ex)

handleLoop ::
  ( loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , MonadTrans q
  , Monad (q m)
  , Liftable xs loopRes
  ) => STrans q m sp loopRes rs ex f () -> STrans q m sp xs ('[]) ex (HandleLoopFunc f) () 
handleLoop hnd = 
  refunc $ composeT 
            (extendForHandlerLoop hnd) 
            (forever (alignWithHandler hnd))

refunc :: (Eval (f sp xs) ~ Eval (f1 sp xs)) => STrans q m sp xs rs ex f a -> STrans q m sp xs rs ex f1 a
refunc = coerce

unsafeRefunc :: STrans q m sp xs rs ex f a -> STrans q m sp xs rs ex f1 a
unsafeRefunc = coerce

pre :: 
  ( cx xs m
  , ConcatExs (Eval (f sp xs)) '[] ~ Eval (f sp xs)
  , Monad (q m)
  ) => STrans q m sp xs xs '[] (OpInterFunc cx) () -> STrans q m sp xs rs ex f () -> STrans q m sp xs rs ex f ()
pre t1 t2 = refunc $ composeT t1 t2

class ('(rs,ex) ~ Eval (f sp xs)) => NextSteps (steps :: NatOne) m sp xs rs ex f | steps sp xs -> rs ex f where
  nextSteps :: Proxy steps -> STrans (ContT Bool) m sp xs rs ex f ()

instance 
  ( Transition m (TransWrap xs) 
  , SplicC sp rs ex zs
  , zs ~ NextStates (TransWrap xs)
  ) => NextSteps One m sp xs rs ex GetNextAllFunc where  
    nextSteps _px = nextEv'

instance 
  ( NextSteps n m sp xs rs_n ex_n func_n
  , Transition m (TransWrap rs_n) 
  , SplicC sp rs_n1 ex_n1 zs_n1
  , zs_n1 ~ NextStates (TransWrap rs_n)
  , KnownNat (Length ex_n)
  , ex_nn1 ~ Concat ex_n ex_n1
  , '(rs_n1, ex_nn1) ~ Eval (ComposeFunc func_n GetNextAllFunc sp xs)
  --) => NextSteps (Succ n) m sp xs rs_n1 ex_nn1 (NextStepsFunc (Succ n)) where --  (ComposeFunc func_n GetNextAllFunc) where  
  ) => NextSteps (Succ n) m sp xs rs_n1 ex_nn1 (ComposeFunc func_n GetNextAllFunc) where  
      nextSteps _px = composeT (nextSteps (Proxy @n)) nextEv'

refuncNextSteps :: (NextSteps steps m sp xs rs ex f, Eval (f sp xs) ~ Eval (NextStepsFunc steps sp xs)) =>
  STrans (ContT Bool) m sp xs rs ex f () -> STrans (ContT Bool) m sp xs rs ex (NextStepsFunc steps) ()
refuncNextSteps = refunc

nextSteps' :: (NextSteps steps m sp xs rs ex f, Eval (f sp xs) ~ Eval (NextStepsFunc steps sp xs)) =>
  Proxy steps -> STrans (ContT Bool) m sp xs rs ex (NextStepsFunc steps) ()
nextSteps' = refuncNextSteps . nextSteps 

skipT :: (steps ~ TotalSteps sp xs GetNextAllFunc, NextSteps steps m sp xs rs ex f_n, Eval (SkipFunc sp xs) ~ '(rs,ex)) => STrans (ContT Bool) m sp xs rs ex SkipFunc ()
skipT = refunc (bindT whatSteps nextSteps)

--
execTrans' :: 
 ( MonadTrans q
 , Monad (q m)
 --, Eval (sfunc NoSplitter '[()]) ~ '(rs,ex)
 ) => STrans q m NoSplitter '[()] rs ex sfunc a -> MFlowExA q m rs ex a
execTrans' t = runTrans t NoSplitter (variantFromValue ())

execTrans_ :: 
  ( MonadTrans q
  , Monad (q m)
  -- , Eval (sfunc NoSplitter '[()]) ~ '(rs,'[])
  ) => STrans q m NoSplitter '[()] rs ('[]) sfunc () -> MFlow q m rs 
execTrans_ t = do 
  ei <- runTrans t NoSplitter (variantFromValue ())
  case ei of
    Right (v_rs,()) ->  return v_rs
    Left _ -> SafeUndef.undefined 
 -- cannot happen as ex ~ '[]

type ExecutableFunc sfunc = Eval (sfunc NoSplitter '[()]) ~ '(('[()]),'[])    
type ExecutableTrans q m sfunc = STrans q m NoSplitter '[()] '[()]  ('[])  sfunc ()
type ExecutableApp q m = STransApp q m NoSplitter '[()] '[()]  ('[]) ()

type AsyncTrans m sp xs rs ex func a = STrans (ContT Bool) m sp xs rs ex a  
type SyncTrans m sp xs rs ex func a = STrans IdentityT m sp xs rs ex a  

type AsyncApp m xs ys a = STransApp (ContT Bool) m NoSplitter xs ys ('[]) a

execTrans :: forall sfunc q m.  
  ( MonadTrans q
  --, ExecutableFunc sfunc
  , Monad (q m)
  ) => ExecutableTrans q m sfunc -> q m () 
execTrans t = fmap variantToValue (execTrans_ t)  
    
execApp ::  
  ( MonadTrans q
  --, ExecutableFunc sfunc
  , Monad (q m)
  ) => ExecutableApp q m  -> q m () 
execApp (MkApp trns) = execTrans trns  

extractKleisliT :: (Monad (q m),Eval (sfunc NoSplitter xs) ~ '(rs,'[])) => STrans q m NoSplitter xs rs '[] sfunc () -> Kleisli (q m) (V xs) (V rs)
extractKleisliT (STrans t) = Kleisli (\v_xs -> do
  ei <- t NoSplitter v_xs
  case ei of
    Right (v_rs,()) -> return v_rs
    Left _ -> SafeUndef.undefined) 

extractKleisli :: (Monad m,Eval (sfunc NoSplitter xs) ~ '(rs,'[])) => STrans IdentityT m NoSplitter xs rs '[] sfunc () -> Kleisli m (V xs) (V rs)
extractKleisli (STrans t) = Kleisli (\v_xs -> runIdentityT $ do
  ei <- t NoSplitter v_xs
  case ei of
    Right (v_rs,()) -> return v_rs
    Left _ -> SafeUndef.undefined) 


extractHandler :: forall m sfunc x rs. (Monad m,Eval (sfunc NoSplitter '[x]) ~ '(rs,'[])) => STrans IdentityT m NoSplitter '[x] rs '[] sfunc () -> (x -> m (V rs))
extractHandler (STrans t) x = runIdentityT $ do
  ei <- t NoSplitter (variantFromValue x)
  case ei of
    Right (v_rs,()) -> return v_rs
    Left _ -> SafeUndef.undefined 



instance (Monad (q m)) => STransMonad (STrans q m) where
  type Cx (STrans q m) sp xs rs ex f = (KnownNat (Length ex)) -- ()

  st_return = returnT   
  st_compose = composeT
  st_bind = bindT


