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

module Beseder.Base.Internal.STransIx where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Identity (IdentityT)
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
newtype STrans q (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*]) (sfunc :: * -> [*] -> ([*],[*]) -> *) (a :: *) =
        STrans {runTrans :: sp -> V xs -> q m (Either (V ex) (V rs,a))}

data STransApp q (m :: * -> *) (sp :: *) (xs :: [*]) (rs :: [*]) (ex :: [*])  (a :: *) where
  MkApp :: STrans q m sp xs rs ex func a -> STransApp  q m sp xs rs ex a 
        
--data STransF q (m :: * -> *) (sp :: *) (sfunc :: * -> [*] -> ([*],[*]) -> *) (a :: *) where
--  MkF :: '(rs,ex) ~ Eval (sfunc sp xs) => STrans q m sp xs rs ex sfunc a -> STransF q m sp sfunc a 

returnT :: Monad (q m) => a -> STrans q m sp xs xs '[] IDFunc a
returnT a = STrans (\_sp v_xs -> return (Right (v_xs,a)))

bindT :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
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

composeT :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
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
          merge3 (Right v_xs) = liftVariant v_xs
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
  ) => STrans q m sp xs xs ex f Bool -> STrans q m sp xs xs ex f ()
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


gets :: 
  ( GetTypeByNameVar name x xs
  , x ~ St st name
  , Monad (q m)
  ) => Named name -> (x -> a) -> STrans q m sp xs xs ('[]) IDFunc a 
gets named f = STrans (\_sp v_xs -> return $ Right (v_xs , (f (getTypeByNameVar named v_xs)))) 
  
op :: 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  ) => m a -> STrans q m sp xs xs  ('[]) IDFunc a
op ma = 
  STrans (\_sp v_xs -> lift ma >>= (\a -> return $ Right (v_xs,a))) 

opRes :: 
  ( Monad m
  , Monad (q m)
  , MonadTrans q
  , GetTypeByNameVar name x xs
  ) => Named name -> (x -> m a) -> STrans q m sp xs xs ('[]) IDFunc a
opRes named f =
  STrans 
    (\_sp v_xs -> do
      op_res <- lift $ f (getTypeByNameVar named v_xs)
      return $ Right (v_xs, op_res))
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
  ) => IO a -> STrans q m sp xs xs ('[]) IDFunc a
liftIO ioa = op (Protolude.liftIO ioa)

whatNext :: Monad (q m) => STrans q m sp xs xs ('[]) IDFunc (Proxy xs)
whatNext = STrans (\_sp v_xs -> return $ Right (v_xs, proxyOfVar v_xs))

noop :: Monad (q m) => STrans q m sp xs xs ('[]) IDFunc ()
noop = STrans (\_sp v_xs -> return $ Right (v_xs, ()))

newState :: 
  ( Eval (f sp xs) ~ '(xs1,ex)
  , xs2 ~ FilterList xs xs1
  , xs3 ~ FilterList xs2 xs1
  , VariantSplitter xs2 xs3 xs1
  , Liftable xs3 xs
  , Monad (q m)
  ) => STrans q m sp xs xs1 ex f () -> STrans q m sp xs xs2 ex(GetNewStateFunc f) ()
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

type HandleLoopFunc f = 
  ComposeFunc
    (ExtendForLoopFunc f) 
    (ForeverFunc (AlignFunc f))

handleLoop ::
  ( loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Liftable rs loopRes 
  , MonadTrans q
  , Monad (q m)
  , Liftable xs loopRes
  ) => STrans q m sp loopRes rs ex f () -> STrans q m sp xs ('[]) ex (HandleLoopFunc f) () 
handleLoop hnd = 
  composeT 
    (extendForHandlerLoop hnd) 
    (forever (alignWithHandler hnd))

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
  ) => NextSteps (Succ n) m sp xs rs_n1 ex_nn1 (ComposeFunc func_n GetNextAllFunc) where  
    nextSteps _px = composeT (nextSteps (Proxy @n)) nextEv'

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
    Left _ -> undefined -- cannot happen as ex ~ '[]

type ExecutableFunc sfunc = Eval (sfunc NoSplitter '[()]) ~ '(('[()]),'[])    
type ExcecutableTrans q m sfunc = STrans q m NoSplitter '[()] '[()]  ('[])  sfunc ()
type ExcecutableApp q m sfunc = STransApp q m NoSplitter '[()] '[()]  ('[]) ()

type AsyncTrans m sp xs rs ex func a = STrans (ContT Bool) m sp xs rs ex a  
type SyncTrans m sp xs rs ex func a = STrans IdentityT m sp xs rs ex a  

execTrans :: forall sfunc q m.  
  ( MonadTrans q
  --, ExecutableFunc sfunc
  , Monad (q m)
  ) => ExcecutableTrans q m sfunc -> q m () 
execTrans t = fmap variantToValue (execTrans_ t)  
    
execApp ::  
  ( MonadTrans q
  --, ExecutableFunc sfunc
  , Monad (q m)
  ) => ExcecutableApp q m sfunc -> q m () 
execApp (MkApp trns) = execTrans trns  

  {-        
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
    , zs ~ Union xs ys
    , SplicC sp rs ex zs
    , Liftable xs zs
    , Liftable ys zs
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
    , zs ~ Union xs ys
    , Liftable xs zs
    , Liftable ys zs
    , SplicC sp rs ex zs
    , Show req
    , KnownSymbol name
    ) => Named name -> req -> STrans q m sp (x ': ys) '(rs,ex) (InvokeFunc req name) ()
  ClearAllVarTrans ::
    ( TermState m (ClrVar xs), MonadTrans q
    , rs_ex ~ Eval (ClearAllVarFunc sp xs)
    ) => STrans q m sp xs rs_ex ClearAllVarFunc ()
  ComposeTrans :: 
    ( --ex_un ~ Union ex1 ex2
    --, Liftable ex1 ex_un
    --, Liftable ex2 ex_un
    --, Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
    --, Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
     Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b 
    ) => STrans q m sp as '(rs1,ex1) f_a () -> STrans q m sp rs1 '(rs2,ex2) f_b b -> STrans q m sp as '(rs2,ex_un) (ComposeFunc f_a f_b) b
  ComposeDirTrans ::
    ( Eval (f_a sp as) ~ '(as, '[])  --assert 
    , Eval (f_b sp as) ~ '(rs2, ex2) --assert
    ) => STrans q m sp as '(as,'[]) f_a () -> STrans q m sp as '(rs2,ex2) f_b b -> STrans q m sp as '(rs2,ex2) f_b b
  BindTrans ::
    ( Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b
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
    --, Eval (f_sub sp xs_sub) ~ '(rs_sub, ex) -- assert
    ) => sp1 -> STrans q m sp xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs1, ex) (CaptureFunc sp1 f_sub) ()
  CaptureOrElseTrans ::
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
    , GetInstance sp1
    --, Eval (f_sub1 sp xs_sub) ~ '(rs_sub1, ex1) -- assert
    --, Eval (f_sub2 sp ex_sub) ~ '(rs_sub2, ex2) -- assert
    ) => sp1 -> STrans q m sp xs_sub '(rs_sub1,ex1) f_sub1 () -> STrans q m sp ex_sub '(rs_sub2,ex2) f_sub2 () -> STrans q m sp xs '(rs, ex) (CaptureOrElseFunc sp1 f_sub1 f_sub2) ()
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
    --, Eval (f_sub (sp :&& sp1) (ListSplitterRes sp1 xs)) ~ '(rs_sub, ex) --assert
    ) => sp1 -> STrans q m (sp :&& sp1) xs_sub '(rs_sub,ex) f_sub () -> STrans q m sp xs '(rs,ex1) (EmbedFunc sp1 f_sub) ()
  ReturnTrans :: a -> STrans q m sp xs '(xs, '[]) IDFunc a 
  MapTrans :: (a -> b) -> STrans q m sp xs rs_ex func a -> STrans q m sp xs rs_ex (MapFunc func) b 
  AskTrans :: MonadReader a (q m) => STrans q m sp xs '(xs, '[]) AskFunc a 
  AsksTrans :: MonadReader a (q m) => (a -> b) -> STrans q m sp xs '(xs, '[]) AsksFunc b 
  ForeverTrans :: 
    ( Eval (f sp xs) ~ '(xs,ex)
    ) => STrans q m sp xs '(xs,ex) f () -> STrans q m sp xs '(('[]),ex) (ForeverFunc f) ()
  GetNewStateTrans :: 
    ( Eval (f sp xs) ~ '(xs1,ex)
    , xs2 ~ FilterList xs xs1
    , xs3 ~ FilterList xs2 xs1
    , VariantSplitter xs2 xs3 xs1
    , Liftable xs3 xs
    ) => STrans q m sp xs '(xs1,ex) f () -> STrans q m sp xs '(xs2,ex) (GetNewStateFunc f) ()
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
    -- , Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
    ) => Bool -> STrans q m sp xs '(rs1,ex) f1 ()  -> STrans q m sp xs '(rs, ex) (IffFunc f1) ()  
  IfElseTrans ::
    ( ex ~ Union ex1 ex2 
    , Liftable ex1 ex
    , Liftable ex2 ex
    , rs ~ Union rs1 rs2
    , Liftable rs1 rs
    , Liftable rs2 rs
    --, Eval (f1 sp xs) ~ '(rs1, ex1)  --assert 
    --, Eval (f2 sp xs) ~ '(rs2, ex2) --assert
    ) => Bool -> STrans q m sp xs '(rs1,ex1) f1 ()  -> STrans q m sp xs '(rs2,ex2) f2 () -> STrans q m sp xs '(rs, ex) (IfElseFunc f1 f2) ()  
  IfJustTrans ::
    ( rs ~ Union rs1 xs
    , Liftable rs1 rs
    , Liftable xs rs
    , Eval (f1 sp xs) ~ '(rs1, ex)  --assert 
    ) => (Maybe b) -> (b -> STrans q m sp xs '(rs1,ex) f1 ())  -> STrans q m sp xs '(rs, ex) (IfJustFunc f1) ()  
  LiftIOTrans :: MonadIO m => IO a -> STrans q m sp xs '(xs, '[]) IDFunc a
  DictTrans ::
    ( TransDict q m dict keyName xs a
    , Eval (DictFunc name sp xs) ~ '(xs,'[])
    ) => dict -> Named keyName -> STrans q m sp xs '(xs, '[]) (DictFunc keyName) a
  AppWrapperTrans ::
    ( Eval (f sp xs) ~ rs_ex
    ) => STransApp q m sp xs rs_ex a -> STrans q m sp xs rs_ex f a 
  InstrumentTrans ::
    ( MonadTrans q
    , Monad m
    , Eval (c xs m)
    , InstrumentorCs c sp xs m f 
    ) => Instrumentor m c -> STrans q m sp xs rs_ex f a -> STrans q m sp xs rs_ex f a
  InstrumentTransTrans ::  STrans q m sp xs '(xs, '[]) f_id a -> STrans q m sp xs rs_ex f a -> STrans q m sp xs rs_ex f a

-}
{-
applyTrans' :: 
  ( MonadTrans q
  , Monad (q m)
  , ListSplitterRes2 sp (Eval (sfunc sp xs)) ~ '(rs,ex)
  ) => STrans q m sp xs '(rs,ex) sfunc a -> sp -> MFlow q m xs ->  MFlowExA q m rs ex a
applyTrans' = applyTrans 

type ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b =
  ( ex_un ~ Concat ex1 ex2
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , MonadTrans q
  , Monad (q m)
  , KnownNat (Length ex1)
  )

class ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b => Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b where
    compose :: STrans q m sp as '(rs1,ex1) f_a () -> STrans q m sp rs1 '(rs2,ex2) f_b b -> sp -> MFlow q m as ->  MFlowExA q m rs2 ex_un b  

instance ComposerC q m sp as rs1 '[] f_a rs2 ex2 f_b ex2 b => 
  Composer q m sp as rs1 '[] f_a rs2 ex2 f_b ex2 b where
    compose t1 t2 sp curSnap =  
      applyTrans t2 sp (fmap (\(Right (v_as,())) -> v_as) (applyTrans t1 sp curSnap))

instance 
  ( ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b
  , ex1 ~ (e ': ex1tail)
  ) => Composer q m sp as rs1 (e ': ex1tail) f_a rs2 ex2 f_b ex_un b where
    compose t1 t2 sp curSnap = do 
      v_as1 <- applyTrans t1 sp curSnap
      ei_res <- case v_as1 of 
        Right (v_bs,()) ->
          let 
            mapResult :: Either (V ex2) (V rs2, b) -> Either (Either (V ex1) (V ex2)) (V rs2, b)
            mapResult (Right r) = Right r 
            mapResult (Left ex2) = Left (Right ex2 :: Either (V ex1) (V ex2))
          in fmap mapResult (applyTrans t2 sp (return v_bs))
        Left v_ex1 -> return $ Left (Left v_ex1  :: Either (V ex1) (V ex2))
      return (  
        case ei_res of
          Right v_rs2 -> Right v_rs2
          Left ei_ex1_ex2 -> Left $ concatEither ei_ex1_ex2)    

--
class ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b => Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b where
    bind :: STrans q m sp as '(rs1,ex1) f_a a -> (a -> STrans q m sp rs1 '(rs2,ex2) f_b b) -> sp -> MFlow q m as ->  MFlowExA q m rs2 ex_un b  

instance ComposerC q m sp as rs1 '[] f_a rs2 ex2 f_b ex2 b => 
  Binder q m sp as rs1 '[] f_a a rs2 ex2 f_b ex2 b where
    bind t1 f_t2 sp curSnap =  
      applyTrans t1 sp curSnap >>= (\(Right (v_as,a)) -> applyTrans (f_t2 a) sp (return v_as))

instance 
  ( ComposerC q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b
  , ex1 ~ (e ': etail)
  ) => Binder q m sp as rs1 (e ': etail) f_a a rs2 ex2 f_b ex_un b where
    bind t1 f_t2 sp curSnap = do 
      v_as1 <- applyTrans t1 sp curSnap
      ei_res <- case v_as1 of 
        Right (v_bs,a) ->
          let 
            mapResult :: Either (V ex2) (V rs2, b) -> Either (Either (V ex1) (V ex2)) (V rs2, b)
            mapResult (Right r) = Right r 
            mapResult (Left ex2) = Left (Right ex2 :: Either (V ex1) (V ex2))
          in fmap mapResult (applyTrans (f_t2 a) sp (return v_bs))
        Left v_ex1 -> return $ Left (Left v_ex1  :: Either (V ex1) (V ex2))
      return (  
        case ei_res of
          Right v_rs2 -> Right v_rs2
          Left ei_ex1_ex2 -> Left $ concatEither ei_ex1_ex2)    
        
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
applyTrans (InvokeTrans named req) sp curSnap = splitV_ sp <$> andReq' named req curSnap
applyTrans (ComposeTrans t1 t2) sp curSnap = compose t1 t2 sp curSnap 
applyTrans (ComposeDirTrans t1 t2) sp curSnap =  
  applyTrans t2 sp (fmap (\(Right (v_as,())) -> v_as) (applyTrans t1 sp curSnap))
applyTrans (BindTrans t1 f_t2) sp curSnap = 
  bind t1 f_t2 sp curSnap
applyTrans (BindDirTrans t1 f_t2) sp curSnap = 
  applyTrans t1 sp curSnap >>= (\(Right (v_as,a)) -> applyTrans (f_t2 a) sp (return v_as))
applyTrans (ReturnTrans a) sp curSnap = fmap (\v_xs -> Right (v_xs,a)) curSnap
applyTrans (MapTrans f t) sp curSnap = do
  ei_xs_ex <- applyTrans t sp curSnap 
  case ei_xs_ex of 
    Right (v_xs, a) -> return $ Right (v_xs, f a)
    Left v_ex -> return $ Left v_ex
applyTrans AskTrans sp curSnap = do 
  v_xs <- curSnap
  a <- ask 
  return $ Right (v_xs,a)
applyTrans (AsksTrans f) sp curSnap = do 
    v_xs <- curSnap
    b <- asks f
    return $ Right (v_xs,b)
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
applyTrans (CaptureOrElseTrans sp1 t1 t2) sp curSnap = do 
  v_xs <- curSnap
  case splitV sp1 v_xs of
    Right v_xs1 -> 
      fmap liftRes (applyTrans t1 sp (return v_xs1))
    Left v_ex -> 
      fmap liftRes (applyTrans t2 sp (return v_ex))
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
applyTrans (GetNewStateTrans t) sp curSnap = do 
  ei_xs_ex <- applyTrans t sp curSnap 
  case ei_xs_ex of 
    Right (v_xs1, ()) -> do
      let px_xs = pxFromFlow curSnap
          px_xs2 = proxyOfFilter v_xs1 px_xs
      case splitVar1' v_xs1 px_xs2 of
        Left v_xs2 -> return (Right (v_xs2,()))
        Right v_xs3 -> applyTrans (GetNewStateTrans t) sp (return (liftVariant v_xs3))   
    Left v_ex -> return $ Left v_ex    
applyTrans WhatNextTrans sp curSnap = fmap (\v->Right (v,Proxy)) curSnap  
applyTrans WhatSplitterTrans sp curSnap = fmap (\v->Right (v,Proxy)) curSnap
applyTrans NoopTrans sp curSnap = fmap (\v->Right (v,())) curSnap
applyTrans GetNextTrans sp curSnap = splitV_ sp <$> andNext curSnap      
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
applyTrans (IfJustTrans (Just b) tf) sp curSnap = 
  fmap liftRightRes (applyTrans (tf b) sp curSnap)
applyTrans (IfJustTrans Nothing tf) sp curSnap = 
  fmap (\v-> Right (liftVariant v,())) curSnap
applyTrans (DictTrans dict named) sp curSnap = applyTransApp (getTransFromDict dict named) sp curSnap
applyTrans (AppWrapperTrans app) sp curSnap = applyTransApp app sp curSnap
applyTrans (InstrumentTrans (Instrumentor fnc) t) sp curSnap = do 
  v_xs <- curSnap
  lift $ fnc v_xs
  applyTrans t sp (return v_xs)
applyTrans (InstrumentTransTrans t0 t) sp curSnap = do 
  v_xs_u <- applyTrans t0 sp curSnap
  let (Right (v_xs,a)) = v_xs_u
  applyTrans t sp (return v_xs)
     

liftRes :: (Liftable rs1 rs, Liftable ex1 ex) => Either (V ex1) (V rs1,()) -> Either (V ex) (V rs,())
liftRes (Right (v_rs1, ())) = Right $ (liftVariant v_rs1, ())        
liftRes (Left v_ex1) = Left $ liftVariant v_ex1

liftRightRes :: (Liftable rs1 rs) => Either (V ex) (V rs1,()) -> Either (V ex) (V rs,())
liftRightRes (Right (v_rs1, ())) = Right $ (liftVariant v_rs1, ())        
liftRightRes (Left v_ex) = Left v_ex




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

type family FinePrintTransFunc (func :: * -> [*] -> ([*],[*]) -> *) where 
  FinePrintTransFunc (ComposeFunc f1 f2) = FinePrintTransFunc f1 ':$$: FinePrintTransFunc f1
  FinePrintTransFunc GetNextAllFunc = 'Text "NextEv" 
  FinePrintTransFunc (NewResFunc name resPars m) = 'Text "NewResource " ':<>: 'Text name ':<>: 'ShowType resPars   
  FinePrintTransFunc (InvokeAllFunc req name) = 'Text "Invoke " ':<>: 'Text name ':<>: 'ShowType req   
  FinePrintTransFunc (ClearAllFunc name) = 'Text "Clear " ':<>: 'Text name 

-}
--printFinely :: TypeError (FinePrintTransFunc f) => Proxy f -> IO ()
--printFinely prx = return ()

  {-
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
  -}
