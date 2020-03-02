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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures      #-}

module Beseder.Base.Internal.Flow where

import           Protolude                    hiding (Product, handle)
import           Control.Monad.Trans
import           Haskus.Utils.ContFlow
import           Haskus.Utils.Flow
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Cont
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Utils.VariantHelper
import           Beseder.Utils.ListHelper
import qualified Prelude as SafeUndef (undefined) 

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

type MFlow' (q ::  ((* -> *) -> * -> *)) (m :: * -> *)  = q m
type MFlow q m  (a :: [*]) = MFlow' q m (Variant a)
type MFlowA q m (xs :: [*]) a =  MFlow' q m  (Variant xs, a)
type MFlowEx q m (xs :: [*]) (ex :: [*]) = MFlow' q m (Either (Variant ex) (Variant xs))
type MFlowExA q m  (xs :: [*]) (ex :: [*]) a = MFlow' q m (Either (Variant ex) (Variant xs, a))

newRes :: (KnownSymbol name, MonadTrans q, CreateRes m name res (V '[x])) => Named name -> res -> MFlow q m '[x]
newRes = contRes

addExistingRes :: (MonadTrans q, Monad (q m), KnownNat (Length sx), KnownNat (Length '[res]), prod ~ Product sx '[res]) =>
  res -> MFlow q m sx -> MFlow q m prod
addExistingRes res curFlow = liftM2 productVariant curFlow (return (variantFromValue res))


appendRes :: (KnownSymbol name, MonadTrans q, Monad (q m), CreateRes m name resFact  (V '[res]), AppendToTuple x res) =>
  Named name-> resFact -> x -> MFlow q m '[AppendToTupleResult x res]
appendRes named resFact x = do
  v_res <- newRes named resFact
  let res = variantToValue v_res
  return $ variantFromValue (appendToTuple x res)

andThen ::
  ( MonadTrans q
  , Monad (q m)
  , ContVariant xs
  , LiftCont fs
  , zs ~ ExtractRHS (TupleToList fs)
  , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
  , ks ~ ExtractMonad (MFlow' q m) zs
  , ys ~ FlattenVariant ks
  , Flattenable (Variant ks) (Variant ys)
  , rs ~ Nub ys
  , Liftable ys rs
  ) => fs -> MFlow q m xs -> MFlow q m rs
andThen fs curFlow = -- do
  --xsVar <-curFlow
  --xsVar ~||> fs
  --xsVar >#^> fs
  (>~||>) curFlow fs

andThenSingle ::
  ( MonadTrans q
  , Monad (q m)
  ) => (a->MFlow q m rs) -> MFlow q m '[a] -> MFlow q m rs
andThenSingle f curFlow = do
  s <- curFlow
  let a = variantToValue s
  f a

andWithHead ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys
  ) =>  (x -> MFlow q m xs) -> MFlow q m (x ': ys) -> MFlow q m zs
andWithHead f curFlow = flowBind curFlow f

andWithHeadToTail ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union ys xs
  ) =>  (x -> MFlow q m xs) -> MFlow q m (x ': ys) -> MFlow q m zs
andWithHeadToTail f curFlow = flowBindToTail curFlow f

andWithMatchToHead ::
  ( MonadTrans q
  , Monad (q m)
  , Popable x xs
  , zs ~ Union ys (Filter x xs)
  , Liftable (Filter x xs) zs
  , Liftable ys zs ) =>  (x -> MFlow q m ys) -> MFlow q m xs -> MFlow q m zs
andWithMatchToHead f curFlow = bindPopableToHead curFlow f

andWithMatchToTail ::
  ( MonadTrans q
  , Monad (q m)
  , Popable x xs
  , zs ~ Union (Filter x xs) ys
  , Liftable (Filter x xs) zs
  , Liftable ys zs ) =>  (x -> MFlow q m ys) -> MFlow q m xs -> MFlow q m zs
andWithMatchToTail f curFlow = bindPopableToTail curFlow f

headToTail ::
  ( MonadTrans q
  , Monad (q m)
  , IsSubset '[x] (Concat xs '[x]) ~ 'True
  , IsSubset xs (Concat xs '[x]) ~ 'True
  , LiftVariant xs (Concat xs '[x])
  , LiftVariant '[x] (Concat xs '[x])
  ) => MFlow q m (x ': xs) -> MFlow q m (Concat xs '[x])
headToTail inputFlow = do
  v_xs <- inputFlow
  return $ case popVariantHead v_xs of
    Right x -> liftVariant (variantFromValue x)
    Left v_filtered -> liftVariant v_filtered

repeatFlow ::
  ( MonadTrans q
  , Monad (q m)
  ) => Int -> (MFlow q m xs -> MFlow q m xs) -> MFlow q m xs -> MFlow q m xs
repeatFlow times f flowToRepeat
    | times <=0 = flowToRepeat
    | otherwise =  do
        v_xs <- flowToRepeat
        repeatFlow (times -1) f (f (return v_xs))

repeatFlowEx ::
  ( MonadTrans q
  , Monad (q m)
  , IsSubset ex ex1 ~ 'True 
  , Liftable ex ex1
  , IsSubset ex' ex1 ~ 'True 
  , Liftable ex' ex1
  , ex1 ~ Concat ex ex'
  ) => Int -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlowEx q m xs ex -> MFlowEx q m xs ex1
repeatFlowEx times f flowToRepeat = do
  ei_v_ex_v_xs <- flowToRepeat
  case ei_v_ex_v_xs of 
    Left v_ex' -> return $ Left (liftVariant v_ex')
    Right v_xs -> repeatFlowEx' times f (return v_xs)

repeatFlowEx' ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable ex' ex1
  , IsSubset ex' ex1 ~ 'True 
  ) => Int -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlow q m xs -> MFlowEx q m xs ex1
repeatFlowEx' times f flowToExec 
  | times <=0 = Right <$> flowToExec 
  | otherwise =  do
      ei_v_ex_v_xs' <- f (Right <$> flowToExec)
      case ei_v_ex_v_xs' of
        Left v_ex' -> return (Left $ liftVariant v_ex')
        Right v_xs -> repeatFlowEx' (times-1) f (return v_xs) 
              
foreverFlow ::
    ( MonadTrans q
    , Monad (q m)
    ) => (MFlow q m xs->MFlow q m xs) -> MFlow q m xs -> MFlow q m '[()]
foreverFlow f flowToExec = do 
  v_xs <- flowToExec
  foreverFlow f (f (return v_xs))

foreverFlowEx ::
  ( MonadTrans q
  , Monad (q m)
  , IsSubset ex ex1 ~ 'True 
  , Liftable ex ex1
  , IsSubset ex' ex1 ~ 'True 
  , Liftable ex' ex1
  , ex1 ~ Concat ex ex'
  ) => (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlowEx q m xs ex -> MFlowEx q m '[] ex1
foreverFlowEx f flowToExec = do 
  ei_v_ex_v_xs <- flowToExec
  case ei_v_ex_v_xs of 
              Left v_ex -> return $ Left $ liftVariant v_ex
              Right v_xs -> foreverFlowEx' f (return $ Right v_xs)

foreverFlowEx' ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable ex' ex1
  , IsSubset ex' ex1 ~ 'True 
  ) => (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlowEx q m xs '[()] -> MFlowEx q m '[] ex1
foreverFlowEx' f flowToExec = do 
  ei_v_ex_v_xs <- flowToExec
  case ei_v_ex_v_xs of 
    Left _v_ex -> SafeUndef.undefined -- "v_ex is '[]"
    Right v_xs -> do 
      ei_v_ex_v_xs' <- f (return $ Right v_xs)
      case ei_v_ex_v_xs' of
        Left v_ex' -> return (Left $ liftVariant v_ex')
        Right v_xs1 -> foreverFlowEx' f (return $ Right v_xs1) 

--
repeatForeverOrNever ::
  ( MonadTrans q
  , Monad (q m)
  , IsSubset ex ex1 ~ 'True 
  , Liftable ex ex1
  , IsSubset ex' ex1 ~ 'True 
  , Liftable ex' ex1
  , ex1 ~ Concat ex ex'
  ) => Bool -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlowEx q m xs ex -> MFlowEx q m xs ex1
repeatForeverOrNever fl f flowToRepeat = do
  ei_v_ex_v_xs <- flowToRepeat
  case ei_v_ex_v_xs of 
    Left v_ex' -> return $ Left (liftVariant v_ex')
    Right v_xs -> repeatForeverOrNever' fl f (return v_xs)

repeatForeverOrNever' ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable ex' ex1
  , IsSubset ex' ex1 ~ 'True 
  ) => Bool -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlow q m xs -> MFlowEx q m xs ex1
repeatForeverOrNever' fl f flowToExec 
  | (not fl) = Right <$> flowToExec 
  | otherwise =  do
      ei_v_ex_v_xs' <- f (Right <$> flowToExec)
      case ei_v_ex_v_xs' of
        Left v_ex' -> return (Left $ liftVariant v_ex')
        Right v_xs -> repeatForeverOrNever' fl f (return v_xs) 

--
repeatUntil ::
  ( MonadTrans q
  , Monad (q m)
  , Monad m
  , IsSubset ex ex1 ~ 'True 
  , Liftable ex ex1
  , IsSubset ex' ex1 ~ 'True 
  , Liftable ex' ex1
  , ex1 ~ Concat ex ex'
  ) => m Bool -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlowEx q m xs ex -> MFlowEx q m xs ex1
repeatUntil fl f flowToRepeat = do
  ei_v_ex_v_xs <- flowToRepeat
  case ei_v_ex_v_xs of 
    Left v_ex' -> return $ Left (liftVariant v_ex')
    Right v_xs -> repeatUntil' fl f (return v_xs)

repeatUntil' ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable ex' ex1
  , IsSubset ex' ex1 ~ 'True 
  , Monad m
  ) => m Bool -> (MFlowEx q m xs '[] -> MFlowEx q m xs ex') -> MFlow q m xs -> MFlowEx q m xs ex1
repeatUntil' fl f flowToExec = do
    flContinue <- lift fl
    if (not flContinue)
      then 
        Right <$> flowToExec
      else 
        do
          ei_v_ex_v_xs' <- f (Right <$> flowToExec)
          case ei_v_ex_v_xs' of
            Left v_ex' -> return (Left $ liftVariant v_ex')
            Right v_xs -> repeatUntil' fl f (return v_xs) 

--
extractOrLoop ::
  ( MonadTrans q
  , Monad (q m)
  , Popable a xs
  ) => (MFlow q m (Filter a xs) -> MFlow q m xs) -> MFlow q m xs -> MFlow q m '[a]
extractOrLoop flowToLoop inputFlow = do
  v_xs <- inputFlow
  case popVariant v_xs of
    Right a -> return (variantFromValue a)
    Left v_filtered -> extractOrLoop flowToLoop (flowToLoop (return v_filtered))

extractOrLoopSingle ::
  ( MonadTrans q
  , Monad (q m)
  , Popable a xs
  ) => (MFlow q m '[a] -> MFlow q m xs) -> MFlow q m '[a] -> MFlow q m (Filter a xs)
extractOrLoopSingle f inputFlow = do
  v_xs <- f inputFlow
  case popVariant v_xs of
    Right a -> extractOrLoopSingle f (return (variantFromValue a))
    Left v_filtered -> return v_filtered
  
applyReqByName :: (MonadTrans q, Monad (q m), TT a (TargetByName name a),
    Request m req (TargetByName name a)) =>
    ReqByName name req a
    -> q m (Variant (ReqResult req (TargetByName name a)))
applyReqByName rbn@(ReqByName req _a) =
  contR req (transformForReq (byNameR rbn))

-- request by name on tuple
contRByName ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  ) => (x -> ReqByName name req x) -> x -> MFlow q m xs
contRByName reqByNm = applyReqByName . reqByNm

contRByName' ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  ) => Named name -> req ->  x -> MFlow q m xs
contRByName' named req x = 
  contR req (transformForReq (byName named x)) 

data NamedRequest req name = NamedRequest req (Named name)   
newtype NamedTuple x = NamedTuple x
instance Wrapping NamedTuple where
  wrap = NamedTuple

instance 
  ( TT x (TargetByName name x)
  , MonadIO m
  , Request m req (TargetByName name x)
  ) => Request m  (NamedRequest req name) (NamedTuple x) where 
  type ReqResult (NamedRequest req name) (NamedTuple x) = ReqResult req (TargetByName name x)
  request (NamedRequest req named) (NamedTuple x) = request req (transformForReq (byName named x))  

instance 
  ( Request m req (NamedTuple x) 
  , MonadIO m
  , res ~ Union ('[x]) (ReqResult req (NamedTuple x))
  , Liftable (ReqResult req (NamedTuple x)) res 
  , Liftable ('[x]) res 
  ) => Request m  (Maybe req) (NamedTuple x) where 
  type ReqResult (Maybe req) (NamedTuple x) = Union ('[x]) (ReqResult req (NamedTuple x))
  request (Just req) namedTuple_x = liftVariant <$> (request req namedTuple_x)  
  request Nothing (NamedTuple x) = return $ liftVariant (variantFromValue x)  

instance 
  ( Request m req1 (NamedTuple x) 
  , Request m req2 (NamedTuple x) 
  , MonadIO m
  , res ~ Union (ReqResult req1 (NamedTuple x)) (ReqResult req2 (NamedTuple x))
  , Liftable (ReqResult req1 (NamedTuple x)) res 
  , Liftable (ReqResult req2 (NamedTuple x)) res 
  ) => Request m  (Either req1 req2) (NamedTuple x) where 
  type ReqResult (Either req1 req2) (NamedTuple x) = Union (ReqResult req1 (NamedTuple x)) (ReqResult req2 (NamedTuple x))
  request (Left req1) namedTuple_x = liftVariant <$> (request req1 namedTuple_x)  
  request (Right req2) namedTuple_x = liftVariant <$> (request req2 namedTuple_x)  

instance (Monad m) => Request m  (V '[]) (NamedTuple x) where 
  type ReqResult (V '[]) (NamedTuple x) = '[]
  request _ _namedTuple_x = SafeUndef.undefined 

instance 
  ( Request m req (NamedTuple x) 
  , Request m (V reqs) (NamedTuple x) 
  , MonadIO m
  , res ~ Union (ReqResult req (NamedTuple x)) (ReqResult (V (reqs)) (NamedTuple x))
  , Liftable (ReqResult req (NamedTuple x)) res 
  , Liftable (ReqResult (V reqs) (NamedTuple x)) res 
  ) => Request m  (V (req ': reqs)) (NamedTuple x) where 
  type ReqResult (V (req ': reqs)) (NamedTuple x) = Union (ReqResult req (NamedTuple x)) (ReqResult (V reqs) (NamedTuple x))
  request v_req_reqs namedTuple_x = 
    case popVariantHead v_req_reqs of 
      Right req -> liftVariant <$> request req namedTuple_x
      Left v_reqs -> liftVariant <$> request v_reqs namedTuple_x

data TerminateRes = TerminateRes deriving (Eq,Show)
newtype NamedTupleClr x = NamedTupleClr x
instance Wrapping NamedTupleClr where
  wrap = NamedTupleClr

instance 
  ( TT x (TargetByName name x)
  , MonadIO m
  , ClearableState m (TargetByName name x)
  ) => Request m  (NamedRequest TerminateRes name) (NamedTupleClr x) where 
  type ReqResult (NamedRequest TerminateRes name) (NamedTupleClr x) = '[ClearResult (TargetByName name x)]
  request (NamedRequest TerminateRes named) (NamedTupleClr x) = variantFromValue <$> clearState (transformForReq (byName named x))  
  
--req will be NamedRequest  
wrapVar :: V xs -> VWrap xs NamedTuple
wrapVar = VWrap

wrapVarClr :: V xs -> VWrap xs NamedTupleClr
wrapVarClr = VWrap

contRVar ::
  ( MonadTrans q
  , Monad (q m)
  , Request m req (VWrap xs NamedTuple)
  ) => req -> V xs -> MFlow q m (ReqResult req (VWrap xs NamedTuple))
contRVar req v_xs = 
  let w_v_xs = wrapVar v_xs
  in contR req w_v_xs    

contCVar ::
  ( MonadTrans q
  , Monad (q m)
  , Request m req (VWrap xs NamedTupleClr)
  ) => req -> V xs -> MFlow q m (ReqResult req (VWrap xs NamedTupleClr))
contCVar req v_xs = 
  let w_v_xs = wrapVarClr v_xs
  in contR req w_v_xs    

andReqAll ::
  ( MonadTrans q
  , Monad (q m)
  , Request m (NamedRequest req name) (VWrap xs NamedTuple)
  ) => Named name -> req -> MFlow q m xs -> MFlow q m (ReqResult (NamedRequest req name) (VWrap xs NamedTuple))
andReqAll named req flow_xs = 
  flow_xs >>= contRVar (NamedRequest req named)

andClearAll ::
  ( MonadTrans q
  , Monad (q m)
  , Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  ) => Named name -> MFlow q m xs -> MFlow q m (ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr))
andClearAll named flow_xs = 
  flow_xs >>= contCVar (NamedRequest TerminateRes named)

contRFuncByName ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  ) => Named name -> (x->req) ->  x -> MFlow q m xs
contRFuncByName named x2req x = 
  contR (x2req x) (transformForReq (byName named x)) 

contCByName ::
  ( MonadTrans q
  , Monad (q m)
  , ClearableState m (TargetByName name x)
  , TT x (TargetByName name x)
  ) => Named name -> x -> MFlow q m '[ClearResult (TargetByName name x)]
contCByName named x = fmap variantFromValue (contC named x)

andReq ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys
  ) => (x -> ReqByName name req x) -> MFlow q m (x ': ys) -> MFlow q m zs
andReq reqByNm = -- curFlow =
  andWithHead (contRByName reqByNm) -- curFlow

andReq' ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys
  ) => Named name -> req -> MFlow q m (x ': ys) -> MFlow q m zs
andReq' named req = -- curFlow =
  andWithHead (contRByName' named req) -- curFlow

andReqFunc ::
  ( MonadTrans q
  , Monad (q m)
  , TT x (TargetByName name x)
  , Request m req (TargetByName name x)
  , xs ~ (ReqResult req (TargetByName name x))
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys
  ) => Named name -> (x -> req) -> MFlow q m (x ': ys) -> MFlow q m zs
andReqFunc named x2req = -- curFlow =
  andWithHead (contRFuncByName named x2req) -- curFlow

andNext ::
  ( MonadIO m
  , ExtendStateTrans x
  , Transition m (ExtendedStateTrans x)
  , xs ~ (NextStates (ExtendedStateTrans x))
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys
  ) => ContQ m (V (x ': ys)) -> ContQ m (V zs)
andNext = andWithHead contW'

andNextAll ::
  ( MonadIO m
  , Transition m (TransWrap xs)
  , xs1 ~ (NextStates (TransWrap xs))
  ) => ContQ m (V xs) -> ContQ m (V xs1)
andNextAll c_v_xs = do 
  v_xs <- c_v_xs 
  contW (TransWrap v_xs) 

andRes ::
  ( MonadTrans q
  , Monad (q m)
  , KnownSymbol name
  , CreateRes m name resFact (V '[res])
  , AppendToTuple x res
  , Liftable ys zs
  , Liftable '[AppendToTupleResult x res] zs
  , zs ~ Union '[AppendToTupleResult x res] ys
  ) => Named name -> resFact -> MFlow q m (x ': ys) -> MFlow q m zs
andRes named resFact = andWithHead (appendRes named resFact)

andMkRes ::
  ( MonadTrans q
  , Monad (q m)
  , KnownSymbol name
  , MkRes m resPars
  , res ~ St (ResSt m resPars) name
  , AppendToTuple x res
  , Liftable ys zs
  , Liftable '[AppendToTupleResult x res] zs
  , zs ~ Union '[AppendToTupleResult x res] ys
  ) => Named name -> resPars -> MFlow q m (x ': ys) -> MFlow q m zs
andMkRes named resPars curFlow = do 
  resData <- lift $ mkRes resPars
  let res = stFromName named resData
  andWithHead (\x -> return (variantFromValue (appendToTuple x res))) curFlow

andResAll :: 
  ( MonadTrans q
  , Monad (q m)
  , KnownSymbol name
  , CreateRes m name resFact (V '[res])
  , AppendToTuple (Variant sx) res
  , AppendToTupleResult (Variant sx) res ~ V ys
  ) => Named name -> resFact -> MFlow q m sx -> MFlow q m ys
andResAll named resFact curFlow = do
  v_res <- newRes named resFact
  let res = variantToValue v_res 
  appendResAll res curFlow

andMkResAll :: 
  ( MonadTrans q
  , Monad (q m)
  , KnownSymbol name
  , MkRes m resFact 
  , res ~ (St (ResSt m resFact) name)
  , AppendToTuple (Variant sx) res
  , AppendToTupleResult  (Variant sx) res ~ V ys
  ) => Named name -> resFact -> MFlow q m sx -> MFlow q m ys
andMkResAll named resFact curFlow = do
  resData <- lift $ mkRes resFact
  let res = stFromName named resData
  appendResAll res curFlow  

appendResAll :: 
  ( MonadTrans q
  , Monad (q m)
  , AppendToTuple (Variant sx) res 
  , AppendToTupleResult (Variant sx) res  ~ V ys
  ) => res -> MFlow q m sx -> MFlow q m ys
appendResAll res curFlow = do
  fmap (\v -> appendToTuple v res) curFlow

andClear ::
  ( MonadTrans q
  , Monad (q m)
  , ClearableState m (TargetByName name x)
  , TT x (TargetByName name x)
  , Liftable ys zs
  , Liftable '[ClearResult (TargetByName name x)] zs
  , zs ~ Union '[ClearResult (TargetByName name x)] ys
  ) => Named name -> MFlow q m (x ': ys) -> MFlow q m zs
andClear named = andWithHead (contCByName named)

andPushBack ::
  ( MonadTrans q
  , Monad (q m)
  , IsSubset '[x] (Concat xs '[x]) ~ 'True
  , IsSubset xs (Concat xs '[x]) ~ 'True
  , LiftVariant xs (Concat xs '[x])
  , LiftVariant '[x] (Concat xs '[x])
  ) => MFlow q m (x ': xs) -> MFlow q m (Concat xs '[x])
andPushBack = headToTail

andBind ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union ys xs
  ) => MFlow q m (x ': xs) -> (MFlow q m (x ': xs) -> x -> MFlow q m ys) -> MFlow q m zs
andBind curFlow createNewFlow = ((andWithHead (createNewFlow curFlow)) curFlow)

andLift :: 
  ( MonadTrans q 
  , Monad (q m)
  ) => MFlow q m xs -> q m a -> MFlowA q m xs a
andLift curFlow qa = curFlow >>= contL qa 

andLift_ :: 
  ( MonadTrans q 
  , Monad (q m)
  ) => MFlow q m xs -> q m () -> MFlow q m xs 
andLift_ curFlow qa = do
  qa >> curFlow

andClearVar :: 
  ( MonadTrans q 
  , Monad (q m)
  , TermState m (V xs)
  ) => MFlow q m xs -> MFlowA q m '[()] ()
andClearVar curFlow = fmap (\v_empty->(v_empty,())) (curFlow >>= contClearVar) 

andClearAllVar :: 
  ( MonadTrans q 
  , Monad (q m)
  , TermState m (ClrVar xs)
  ) => MFlow q m xs -> MFlowA q m '[()] ()
andClearAllVar curFlow = fmap (\v_empty->(v_empty,())) (curFlow >>= contClearAllVar) 

andLiftEx :: 
  ( MonadTrans q 
  , Monad (q m)
  ) => MFlowEx q m xs ex -> q m a -> MFlowExA q m xs ex a
andLiftEx curFlow qa = do
  ei_ex_xs <- curFlow
  mapM (contL qa) ei_ex_xs  


andBranch ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union ys xs
  ) => MFlow q m (x ': xs) -> (MFlow q m '[x] -> MFlow q m ys) -> MFlow q m zs
andBranch curFlow branchFunc = do
  v_xs <- curFlow
  case popVariantHead v_xs of
    Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
    Left v_filtered -> return $ liftVariant v_filtered

andBranchBack ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys 
  ) => MFlow q m (x ': xs) -> (MFlow q m '[x] -> MFlow q m ys) -> MFlow q m zs
andBranchBack curFlow branchFunc = do
  v_xs <- curFlow
  case popVariantHead v_xs of
    Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
    Left v_filtered -> return $ liftVariant v_filtered

andBranchTail ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable '[x] zs
  , Liftable ys zs
  , zs ~ Union ys '[x] 
  ) => MFlow q m (x ': xs) -> (MFlow q m xs -> MFlow q m ys) -> MFlow q m zs
andBranchTail curFlow branchFunc = do
  v_xs <- curFlow
  case popVariantHead v_xs of
    Right x -> return $ liftVariant $ variantFromValue x
    Left v_filtered -> liftVariant <$> branchFunc (return v_filtered)

andSplit ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable ts zs
  , Liftable ys zs
  , zs ~ Union ys ts
  ) => MFlow q m (x ': xs) -> (MFlow q m '[x] -> MFlow q m ys) -> (MFlow q m xs -> MFlow q m ts) -> MFlow q m zs
andSplit curFlow branchFunc branchFuncTail = do
  v_xs <- curFlow
  case popVariantHead v_xs of
    Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
    Left v_xs1 -> liftVariant <$> branchFuncTail (return v_xs1)
  
andPushAside ::
  ( MonadTrans q
  , Monad (q m)
  ) => MFlow q m (x ': xs) -> (MFlow q m xs -> MFlow q m ys) -> MFlow q m '[V '[x] , (V ys)] 
andPushAside curFlow branchFunc = do
  v_xs <- curFlow
  case popVariantHead v_xs of
    Right x -> return $ toVariantAt @0 (variantFromValue x)
    Left v_filtered -> toVariantAt @1 <$> branchFunc (return v_filtered)

andFlatten2 ::
  ( MonadTrans q
  , Monad (q m)
  , Liftable xs zs
  , Liftable ys zs
  , zs ~ Union xs ys  
  ) => MFlow q m [V (xs) , (V ys)]  -> MFlow q m zs 
andFlatten2 curFlow = do
  v_xs <- curFlow
  let ei = variantToEither v_xs
  case ei of
    Right v_xs1 -> return $ liftVariant v_xs1
    Left v_ys -> return $ liftVariant v_ys

andBranchType :: forall x xs m q zs ys.
  ( MonadTrans q
  , Monad (q m)
  , Popable x xs 
  , Liftable (Filter x xs) zs
  , Liftable ys zs
  , zs ~ Union ys xs
  ) => MFlow q m xs -> (MFlow q m '[x] -> MFlow q m ys) -> MFlow q m zs
andBranchType curFlow branchFunc = do
 v_xs <- curFlow
 case popVariant v_xs of
   Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
   Left v_filtered -> return $ liftVariant v_filtered

andBranchNestedType :: forall n x xs m q zs ys.
   ( MonadTrans q
   , Monad (q m)
   , x ~ ByNestedType n xs
   , Popable x xs 
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union ys (Filter x xs)
   ) => MFlow q m xs -> Proxy n -> (MFlow q m '[x] -> MFlow q m ys) -> MFlow q m zs
andBranchNestedType curFlow _px branchFunc = do
  v_xs <- curFlow
  case popVariant v_xs of
    Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
    Left v_filtered -> return $ liftVariant v_filtered

andBranchButNestedType :: forall n x xs m q zs ys as.
    ( MonadTrans q
    , Monad (q m)
    , x ~ ByNestedType n xs
    , Popable x xs 
    , Liftable '[x] zs
    , Liftable ys zs
    , zs ~ Union ys '[x]
    , as ~ Filter x xs
    ) => MFlow q m xs -> Proxy n -> (MFlow q m as -> MFlow q m ys) -> MFlow q m zs
andBranchButNestedType curFlow _px branchFunc = do
   v_xs <- curFlow
   let popRes:: Either (V as) x 
       popRes = popVariant v_xs  
   case popRes of
     Right x -> return $  liftVariant $ variantFromValue x
     Left v_filtered -> liftVariant <$> branchFunc (return v_filtered)
 
andIfNestedTypeElse :: forall n x xs m q zs ys ts.
    ( MonadTrans q
    , Monad (q m)
    , x ~ ByNestedType n xs
    , Popable x xs 
    , Liftable ys zs
    , Liftable ts zs
    , zs ~ Union ys ts
    ) => MFlow q m xs -> Proxy n -> (MFlow q m '[x] -> MFlow q m ys) -> (MFlow q m (Filter x xs) -> MFlow q m ts) -> MFlow q m zs
andIfNestedTypeElse curFlow _px branchFunc branchFuncElse = do
  v_xs <- curFlow
  let popRes:: Either (V (Filter x xs)) x 
      popRes = popVariant v_xs  
  case popRes of  
    Right x -> liftVariant <$> branchFunc (return $ variantFromValue x)
    Left v_filtered -> liftVariant <$> branchFuncElse (return v_filtered)

andBranchNestedTypes :: forall n as bs xs m q zs ys.
   ( MonadTrans q
   , Monad (q m)
   , as ~ FilterByNestedType n xs 
   , bs ~ FilterList as xs 
   , ListNotEmpty as
   , Liftable bs zs
   , Liftable ys zs
   , zs ~ Union ys bs
   , VariantSplitter as bs xs
   ) => MFlow q m xs -> Proxy n -> (MFlow q m as -> MFlow q m ys) -> MFlow q m zs
andBranchNestedTypes curFlow _px branchFunc = do
  v_xs <- curFlow
  let p_a :: Proxy as
      p_a = Proxy
      p_b :: Proxy bs 
      p_b = Proxy
  case splitVar v_xs p_a p_b of
    Left v_as -> liftVariant <$> branchFunc (return v_as)
    Right v_bs -> return $ liftVariant v_bs

andBranchButNestedTypes :: forall n as bs xs m q zs ys.
    ( MonadTrans q
    , Monad (q m)
    , as ~ FilterByNestedType n xs 
    , bs ~ FilterList as xs 
    , ListNotEmpty bs
    , Liftable as zs
    , Liftable ys zs
    , zs ~ Union as ys
    , VariantSplitter as bs xs
    ) => MFlow q m xs -> Proxy n -> (MFlow q m bs -> MFlow q m ys) -> MFlow q m zs
andBranchButNestedTypes curFlow _px branchFunc = do
   v_xs <- curFlow
   let p_a :: Proxy as
       p_a = Proxy
       p_b :: Proxy bs 
       p_b = Proxy
   case splitVar' v_xs p_a p_b of
     Left v_as -> return $ liftVariant v_as 
     Right v_bs -> liftVariant <$> branchFunc (return v_bs)
   

addUnit :: (Functor (q m)) => MFlow q m xs -> MFlowA q m xs ()
addUnit = fmap (\v_xs->(v_xs,()))

addUnitEx :: Functor (q m) => MFlowEx q m xs ex -> MFlowExA q m xs ex ()
addUnitEx = fmap (fmap (\v_xs->(v_xs,())))

swapEither :: Either a b -> Either b a 
swapEither (Left a) = Right a
swapEither (Right a) = Left a

swapEither' :: Either a (b,()) -> Either b (a,()) 
swapEither' (Left a) = Right (a,())
swapEither' (Right (a,())) = Left a

pxFromFlow :: MFlow q m a -> Proxy a
pxFromFlow _ = Proxy 