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
{-# LANGUAGE ConstraintKinds        #-}

module Beseder.Base.Internal.Core where

import           Control.Concurrent.STM.TVar
import           Haskus.Utils.Types
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Protolude                    hiding (Product, handle,TypeError)
import           Beseder.Utils.VariantHelper
import           Beseder.Base.Internal.Named
import           Data.Function (id)
import           Data.Coerce 
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.TypeExp

-- Base resource operations: 
class (Monad q) => CreateRes q name res state | q name res -> state where  
  createRes :: Named name -> res -> q state

class (Monad q) => MkRes q res  where
  type ResSt q res  :: *
  mkRes :: res -> q (ResSt q res)

class (MonadIO q, StateTrans state1 ~ 'Dynamic) => Transition q state1 where
  type NextStates state1 :: [*]
  type NextStates state1 =  
    TypeError 
      ( 'Text "State `"
        ':<>: 'ShowType state1 ':<>: 'Text "' has no transition defined`")
  next :: state1 -> (V (NextStates state1) -> q Bool) -> q Bool


class  (Monad q) => Request q req state1 where
  type ReqResult (req :: *) (st :: *) :: [*]
  type instance ReqResult req st =  
    TypeError 
      ( 'Text "Request `"
        ':<>: 'ShowType req ':<>: 'Text "' is not implemented for`"
        ':<>: 'ShowType st ':<>: 'Text "'")
  request :: req -> state1 -> q (V (ReqResult req state1))

  
class (Monad q, StateTrans state ~ 'Static ) => TermState q state where
  terminate :: state -> q ()

class (Monad q) => ClearableState q state where
  type ClearResult state
  clearState :: state -> q (ClearResult state)

type family TermRequest (st :: *) :: *

---

data StateTransKind
  = Dynamic
  | Static

type family StateTrans (st :: k) :: StateTransKind

type family PairStateTrans (a::k) (b::k) :: StateTransKind where
  PairStateTrans a b = PairStateTrans' (StateTrans a) (StateTrans b)

type family PairStateTrans' (t1::StateTransKind) (t2::StateTransKind) where
  PairStateTrans' 'Static 'Static = 'Static
  PairStateTrans' _ _ = 'Dynamic

type instance StateTrans (a,b) = PairStateTrans a b
type instance StateTrans (Either a b) = PairStateTrans a b

class ExtendStateTrans st where
  type ExtendedStateTrans st :: *
  extendStateTrans :: st -> ExtendedStateTrans st

newtype St a (name :: Symbol) = St a

nameFromSt :: St a name -> Named name
nameFromSt _ = Named

type family StateName namedState :: Symbol where
  StateName (St a name) = name

instance ExtendStateTrans (St a name) where
  type ExtendedStateTrans (St a name) = St a name-- StateTransData (StateTrans (St a)) (St a)
  extendStateTrans = id -- (St a) = St a  --StateTransData (St a)

newtype StateTransData2 (tk1 :: StateTransKind) (tk2 :: StateTransKind) (a:: *)  (b:: *) = StateTransData2 (a,b)
type instance StateTrans (StateTransData2 tk1 tk2 a b) = PairStateTrans' tk1 tk2 -- 'Dynamic --PairStateTrans' tk1 tk2

newtype StateTransData (tk :: StateTransKind) (a :: *) = StateTransData a
mkStateTransData :: st -> StateTransData (StateTrans st) st
mkStateTransData =  StateTransData

instance ExtendStateTrans (St a n1, St b n2) where
  type ExtendedStateTrans (St a n1, St b n2) = StateTransData2 (StateTrans (St a n1)) (StateTrans (St b n2)) (St a n1) (St b n2)
  extendStateTrans = coerce -- (a,b) = StateTransData2 (a,b)

stFromName :: Named name -> st -> St st name
stFromName _ st = St st

type CanBeTerminated m st  
  = ( MonadIO m
    , Request m (TermRequest st) st
    , GetInstance (TermRequest st) 
    , TermState m (V (ReqResult (TermRequest st) st))  
    )    

instance (MonadIO q, TermState q (St a name)) => ClearableState q (StateTransData 'Static (St a name)) where
  type ClearResult (StateTransData 'Static (St a name)) = ()
  clearState (StateTransData st) = terminate st

instance (MonadIO q, CanBeTerminated q (St a name)) => ClearableState q (StateTransData 'Dynamic (St a name)) where
  type ClearResult (StateTransData 'Dynamic (St a name)) = ()
  clearState (StateTransData st) = terminateDynState st
  
instance (MonadIO q, ClearableState q (StateTransData (StateTrans (St a name)) (St a name))) => ClearableState q (St a name) where
  type ClearResult (St a name) = ClearResult (StateTransData (StateTrans ((St a name))) ((St a name)))
  clearState = clearState . mkStateTransData

terminateDynState :: forall st m. CanBeTerminated m st => st -> m ()
terminateDynState st = 
  let req :: TermRequest st
      req = getInstance
  in request req st >>= terminate
  
type instance StateTrans () = 'Static
instance (MonadIO q) => TermState q () where
  terminate () = return ()
  
type family NextStates2 s1 s2 where
  NextStates2 s1 s2 = Concat (Product '[s1] (NextStates s2)) (Product (NextStates s1) '[s2])


instance (MonadIO m, TermState  m s1, TermState  m s2) => TermState m (StateTransData2 'Static 'Static s1 s2)  where
  terminate (StateTransData2 (a,b)) = terminate a >> terminate b

instance (MonadIO m, TermState m s1, TermState  m s2) => ClearableState m (StateTransData2 'Static 'Static s1 s2)  where
  type ClearResult (StateTransData2 'Static 'Static s1 s2) = ()
  clearState (StateTransData2 (a,b)) = terminate a >> terminate b 
  
instance (MonadIO m, ClearableState  m s1, TermState  m s2) => ClearableState m (StateTransData2 'Dynamic 'Static s1 s2)  where
  type ClearResult (StateTransData2 'Dynamic 'Static s1 s2) = ClearResult s1 --expected to be ()
  clearState (StateTransData2 (a,b)) = terminate b >> clearState a 

instance (MonadIO m, TermState  m s1, ClearableState  m s2) => ClearableState m (StateTransData2 'Static 'Dynamic s1 s2)  where
  type ClearResult (StateTransData2 'Static 'Dynamic s1 s2) = ClearResult s2
  clearState (StateTransData2 (a,b)) = terminate a >> clearState b

instance (MonadIO m, ClearableState m s1, ClearableState  m s2, ClearResult s1 ~ ()) => ClearableState m (StateTransData2 'Dynamic 'Dynamic s1 s2)  where
  type ClearResult (StateTransData2 'Dynamic 'Dynamic s1 s2) = ClearResult s2
  clearState (StateTransData2 (a,b)) = clearState a >> clearState b
    
instance (MonadIO m, CollapseStateTrans s2, Transition m s1, Transition m s2, KnownNat (Length (Product '[s1] (NextStates s2) )), KnownNat (Length (Product (NextStates s1) '[CollapsedStateTrans s2])), KnownNat (Length (NextStates s2)),
  'True ~ IsSubset  (Product (NextStates s1) '[CollapsedStateTrans s2]) (Concat (Product (NextStates s1) '[CollapsedStateTrans s2]) (Product '[s1] (NextStates s2))),
  'True ~ IsSubset  (Product '[s1] (NextStates s2)) (Concat (Product (NextStates s1) '[CollapsedStateTrans s2]) (Product '[s1] (NextStates s2))),
  Liftable  (Product '[s1] (NextStates s2)) (Concat (Product (NextStates s1) '[CollapsedStateTrans s2]) (Product '[s1] (NextStates s2))),
  Liftable  (Product (NextStates s1) '[CollapsedStateTrans s2]) (Concat (Product (NextStates s1) '[CollapsedStateTrans s2]) (Product '[s1] (NextStates s2)))) =>
    Transition m (StateTransData2 'Dynamic 'Dynamic s1 s2)  where
  type NextStates (StateTransData2 'Dynamic 'Dynamic s1 s2) = Concat (Product (NextStates s1) '[CollapsedStateTrans s2]) (Product '[s1] (NextStates s2))
  next (StateTransData2 (s1, s2)) func = next2 (s1,s2) (func . convertEither2)

data TransitionablePair s1 s2 = TransitionablePair s1 s2
type instance StateTrans (TransitionablePair s1 s2) = PairStateTrans s1 s2

instance (MonadIO m, CollapseStateTrans s2, Transition m s1, Transition m s2,  StateTrans (TransitionablePair s1 s2) ~ 'Dynamic,
  CollapseStateTrans s2, 
  KnownNat (Length (Product (NextStates s1) '[CollapsedStateTrans s2])), KnownNat (Length (NextStates s2))) => 
  Transition m (TransitionablePair s1 s2) where
    type NextStates (TransitionablePair s1 s2) = '[Either (s1,V (NextStates s2)) (V (NextStates s1), CollapsedStateTrans s2)]
    next (TransitionablePair s1 s2) func = next2 (s1,s2) (func . variantFromValue)

class CollapseStateTrans st where
  type CollapsedStateTrans st :: *
  collapseStateTrans :: st -> CollapsedStateTrans st

instance ExtendStateTrans ((St a n1, St b n2), St c n3) where
  type ExtendedStateTrans ((St a n1, St b n2), St c n3) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans (St b n2, St c n3))) (St a n1) (ExtendedStateTrans (St b n2, St c n3))
  extendStateTrans ((a,b),c) = StateTransData2 (a, StateTransData2 (b,c))

instance ExtendStateTrans (St a n1, (St b n2, St c n3)) where
  type ExtendedStateTrans (St a n1, (St b n2, St c n3)) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans (St b n2, St c n3))) (St a n1) (ExtendedStateTrans (St b n2, St c n3))
  extendStateTrans (a, (b,c)) = StateTransData2 (a, StateTransData2 (b,c))

instance ExtendStateTrans ((St b n2, St c n3), St d n4) => ExtendStateTrans (((St a n1, St b n2), St c n3), St d n4 ) where
  type ExtendedStateTrans (((St a n1, St b n2), St c n3), St d n4 ) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans ((St b n2, St c n3), St d n4))) (St a n1) (ExtendedStateTrans ((St b n2, St c n3), St d n4))
  extendStateTrans (((a,b),c),d) = StateTransData2 (a, extendStateTrans ((b,c),d))

instance ExtendStateTrans (St b n2, (St c n3, St d n4)) => ExtendStateTrans (St a n1, (St b n2, (St c n3, St d n4))) where
  type ExtendedStateTrans  (St a n1, (St b n2, (St c n3, St d n4))) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans ((St b n2, St c n3), St d n4))) (St a n1) (ExtendedStateTrans ((St b n2, St c n3), St d n4))
  extendStateTrans (a,(b,(c,d))) = StateTransData2 (a, extendStateTrans (b,(c,d)))

instance ExtendStateTrans (St b n2, (St c n3, (St d n4, St e n5))) => ExtendStateTrans (St a n1, (St b n2, (St c n3, (St d n4, St e n5)))) where
  type ExtendedStateTrans  (St a n1, (St b n2, (St c n3, (St d n4, St e n5)))) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans (((St b n2, St c n3), St d n4), St e n5))) (St a n1) (ExtendedStateTrans (((St b n2, St c n3), St d n4), St e n5))
  extendStateTrans (a,(b,(c,(d,e)))) = StateTransData2 (a, extendStateTrans (b,(c,(d,e))))

instance ExtendStateTrans (St b n2, (St c n3, (St d n4, (St e n5, St f n6)))) => ExtendStateTrans (St a n1, (St b n2, (St c n3, (St d n4, (St e n5, St f n6))))) where
  type ExtendedStateTrans  (St a n1, (St b n2, (St c n3, (St d n4, (St e n5, St f n6))))) = StateTransData2 (StateTrans (St a n1)) (StateTrans (ExtendedStateTrans (St b n2, (St c n3, (St d n4, (St e n5, St f n6)))))) (St a n1) (ExtendedStateTrans ((St b n2, (St c n3, (St d n4, (St e n5, St f n6))))))
  extendStateTrans (a,(b,(c,(d,(e,f))))) = StateTransData2 (a, extendStateTrans (b,(c,(d,(e,f)))))
  --TODO: add one more to support longer tuples
  --IDEA: Use template Haskell for that

instance CollapseStateTrans (St a n) where
  type CollapsedStateTrans (St a n) = St a n
  collapseStateTrans = id -- st = st

instance (CollapseStateTrans s2) => CollapseStateTrans (StateTransData2 tk1 tk2 s1 s2) where
  type CollapsedStateTrans (StateTransData2 tk1 tk2 s1 s2) = (s1, CollapsedStateTrans s2)
  collapseStateTrans (StateTransData2 (s1,s2)) = (s1, collapseStateTrans s2)

next2 :: (MonadIO m, CollapseStateTrans state2, Transition m state1, Transition m state2, KnownNat (Length (Product (NextStates state1) '[CollapsedStateTrans state2])), KnownNat (Length (NextStates state2))) =>
    (state1,state2) -> (Either (state1,V (NextStates state2)) (V (NextStates state1), CollapsedStateTrans state2) -> m Bool) -> m Bool
next2 (state1, state2) func = do
    flag <- liftIO $ newTVarIO True
    _ <- next state1 (\state1Next ->
        do
          curFlag <- liftIO $ atomically $ readTVar flag
          if curFlag
              then do
                  liftIO $ atomically $ writeTVar flag False
                  func (Right (state1Next,collapseStateTrans state2))
              else
                return True)
    _ <- next state2 (\state2Next ->
        do
          curFlag <- liftIO $ atomically $ readTVar flag
          if curFlag
              then do
                  liftIO $ atomically $ writeTVar flag False
                  func (Left (state1,state2Next))
              else
                return True)
    return True

instance (MonadIO m, CollapseStateTrans s1, TermState m s1, Transition m s2, KnownNat (Length (NextStates s2))) => Transition m (StateTransData2 'Static 'Dynamic s1 s2)  where
  type NextStates (StateTransData2 'Static 'Dynamic s1 s2) =  Product '[CollapsedStateTrans s1] (NextStates s2)
  next (StateTransData2 (s1, s2)) func = next s2 (\s2res -> func (productVariant (variantFromValue (collapseStateTrans s1)) s2res))

instance (MonadIO m, Transition m s1, CollapseStateTrans s2 ,TermState m s2) => Transition m (StateTransData2 'Dynamic 'Static s1 s2)  where
  type NextStates (StateTransData2 'Dynamic 'Static s1 s2) =  Product (NextStates s1) '[CollapsedStateTrans s2]
  next (StateTransData2 (s1, s2)) func = next s1 (\s1res -> func (productVariant s1res (variantFromValue (collapseStateTrans s2))))


instance (Monad m, TermState m s1, TermState m s2) => TermState m (s1,s2) where
  terminate (s1 , s2) = do
    terminate s1
    terminate s2

instance (Monad m, Request m req state1) => Request m req (state1,state2) where
  type ReqResult req (state1,state2) = Product (ReqResult req state1) '[state2]
  request  req  (state1, state2) = do
    state1Res <- request req state1
    return $ productVariant state1Res (variantFromValue state2)

{-
instance 
  ( Monad m
  , Request m req1 state
  , Request m req2 state
  , res ~ Union (ReqResult req1 state) (ReqResult req2 state)
  , Liftable (ReqResult req1 state) res
  , Liftable (ReqResult req2 state) res
  ) => Request m (Either req1 req2) state where
  type ReqResult (Either req1 req2) state = Union (ReqResult req1 state) (ReqResult req2 state)
  request  (Left req1)  state = liftVariant <$> request req1 state
  request  (Right req2)  state = liftVariant <$> request req2 state

instance 
  ( Monad m
  , state ~ St st name
  , Request m req state
  , res ~ Union ('[state]) (ReqResult req state)
  , Liftable ('[state]) res
  , Liftable (ReqResult req state) res
  ) => Request m (Maybe req) state where
  type ReqResult (Maybe req) (St st name) = Union ('[St st name]) (ReqResult req (St st name))
  request  (Just req)  state = liftVariant <$> request req state
  request  Nothing  state = return $ liftVariant (variantFromValue state)
-}

instance (MonadIO m, ClearableState m state1) => ClearableState m (state1,state2) where
  type ClearResult (state1,state2) = state2
  clearState (state1,state2) =
    clearState state1 >> return state2

newtype RT a b = RT (a,b) --reverse tuple
instance (Monad m, Request m req state2, KnownNat (Length (ReqResult req state2))) => Request m req (RT state1 state2) where
  type ReqResult req (RT state1 state2) = Product '[state1] (ReqResult req state2)
  request  req  (RT (state1, state2)) = do
    state2Res <- request req state2
    return $ productVariant (variantFromValue state1) state2Res

-------- Truncate
class TruncateTuple t where
  type TruncatedTuple t
  truncateTuple :: t -> TruncatedTuple t

instance  {-# OVERLAPS #-} TruncateTuple (()) where
  type TruncatedTuple () = ()
  truncateTuple  () = ()

instance  {-# OVERLAPS #-} TruncateTuple (St a name) where
  type TruncatedTuple (St a name) = St a name
  truncateTuple  a = a

instance  {-# OVERLAPS #-} TruncateTuple (a, ()) where
  type TruncatedTuple (a, ()) = a
  truncateTuple  (a,()) = a

instance  {-# OVERLAPS #-} TruncateTuple (a, St b name) where
  type TruncatedTuple (a, St b name) = (a, St b name)
  truncateTuple = id -- (a,b) = (a,b)

instance  {-# OVERLAPS #-} TruncateTuple (a, (b,c)) where
  type TruncatedTuple (a, (b,c)) = (a,(b,c))
  truncateTuple = id --  a = a

instance (MonadIO m, ClearableState m state2, TruncateTuple (state1, ClearResult state2)) => ClearableState m (RT state1 state2) where
  type ClearResult (RT state1 state2) = TruncatedTuple (state1,  ClearResult state2)
  clearState (RT (state1,state2)) = do
    cleared <- clearState state2
    return $ truncateTuple (state1, cleared)

instance (Monad m, TermState m s1, TermState m s2) => TermState m (Either s1 s2) where
  terminate (Left s1) = terminate s1
  terminate (Right s2) = terminate s2

instance (MonadIO m) => TermState m (V '[]) where
  terminate _ = return ()
instance (MonadIO m, TermState m (Variant xs), TermState m x) => TermState m (Variant (x ': xs)) where
  terminate v = case popVariantHead v of
      Right x -> terminate x
      Left xs -> terminate xs

newtype ClrVar xs = ClrVar (V xs)
type instance StateTrans (ClrVar xs) = 'Static
instance (MonadIO m) => TermState m (ClrVar '[]) where
  terminate _ = return ()

instance 
   ( MonadIO m
   , TermState m (ClrVar xs)
   , ExtendStateTrans x
   , ClearableState m (ExtendedStateTrans x)
   , ClearResult (ExtendedStateTrans x)~()) => TermState m (ClrVar (x ': xs)) where
  terminate (ClrVar v) = case popVariantHead v of
      Right x -> clearState (extendStateTrans x)
      Left xs -> terminate (ClrVar xs)

instance (MonadIO m) => ClearableState m (V '[]) where
  type ClearResult (V '[])= ()
  clearState _ = return ()

type instance  StateTrans (V '[]) = 'Static
type instance  StateTrans (V (x ': xs)) = PairStateTrans' (StateTrans x) (StateTrans (V xs))

--type family ToString (a::k) :: Symbol
--type instance ToString (St a name) = AppendSymbol (AppendSymbol (ToString a) "#") name

----
newtype VWrap xs (w :: * -> *) = VWrap (V xs)
class Wrapping (w :: * -> *) where
  wrap :: x -> w x

v_wrap :: (Wrapping w) => VWrap xs w -> x -> w x
v_wrap _ = wrap

newVWrap :: VWrap xs w -> V xs1 -> VWrap xs1 w 
newVWrap _ = VWrap

instance (MonadIO q) => Request q req (VWrap '[] w) where
  type ReqResult req (VWrap '[] w) = '[]
  request _req _vempty = undefined

instance 
  ( MonadIO q
  , Wrapping w
  , Request q req (w x)
  , Request q req (VWrap xs w)
  , Liftable (ReqResult req (w x)) (Union (ReqResult req (w x)) (ReqResult req (VWrap xs w)))
  , Liftable (ReqResult req (VWrap xs w)) (Union (ReqResult req (w x)) (ReqResult req (VWrap xs w)))
  ) => Request q req (VWrap (x ': xs) w) where
    type ReqResult req (VWrap (x ': xs) w) = Union (ReqResult req (w x)) (ReqResult req (VWrap xs w)) 
    request req vw@(VWrap v_x_xs) = 
      case popVariantHead v_x_xs of 
        Right x -> liftVariant <$> request req (v_wrap vw x)
        Left v_xs -> liftVariant <$> request req (newVWrap vw v_xs)
  

        
newtype TransWrap xs = TransWrap (V xs)        
type instance  StateTrans (TransWrap xs) = 'Dynamic
--type instance  StateTrans (V (x ': xs)) = PairStateTrans' (StateTrans x) (StateTrans (V xs))

instance (MonadIO m) => Transition m (TransWrap '[]) where
  type NextStates (TransWrap '[]) = '[]
  next _req _t_empty = undefined

instance 
  ( MonadIO m
  , ExtendStateTrans x
  , Transition m (TransWrap xs)
  , Transition m (ExtendedStateTrans x)
  , Liftable (NextStates (ExtendedStateTrans x)) (Union (NextStates (ExtendedStateTrans x)) (NextStates (TransWrap xs)))
  , Liftable (NextStates (TransWrap xs)) (Union (NextStates (ExtendedStateTrans x)) (NextStates (TransWrap xs)))
  ) => Transition m (TransWrap (x ': xs)) where
  type NextStates (TransWrap (x ': xs)) = Union (NextStates (ExtendedStateTrans x)) (NextStates (TransWrap xs))
  next (TransWrap v_x_xs) cb = 
    case popVariantHead v_x_xs of 
      Right x -> next (extendStateTrans x) (\v_x_next -> cb (liftVariant v_x_next))
      Left v_xs -> next (TransWrap v_xs)  (\v_xs_next -> cb (liftVariant v_xs_next))

type instance  StateTrans (St () name) = 'Static      
instance (MonadIO m) => TermState m (St () name) where
  terminate _ = return ()

type family IsStateEmptyFam a :: Bool where
  IsStateEmptyFam (St () _) = 'True
  IsStateEmptyFam _ = 'False
  
data IsStateEmpty :: Type -> Exp Bool 
type instance Eval (IsStateEmpty a) = IsStateEmptyFam a
  
