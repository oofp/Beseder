{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}

module Beseder.Utils.VariantHelper where

import           Haskus.Utils.ContFlow
import           Haskus.Utils.Flow       hiding ((|>))
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Protolude               hiding (Product)
import           Beseder.Utils.ListHelper

convertEither ::
  ( --Productable (V s1x)   (V '[s2])
  -- , Productable (V '[s1]) (V s2x)
  KnownNat (Length s2x)
  , KnownNat (Length (Product '[s1] s2x))
  , KnownNat (Length (Product s1x '[s2]))
  ) => Either (s1,V s2x) (V s1x, s2)
  -> V (FlattenVariant '[V (Product s1x '[s2]), V (Product '[s1] s2x)])
convertEither = flattenVariant . variantFromEither . bimap convertLeft convertRight
  where
    convertLeft  (s,vs) = productVariant (variantFromValue s) vs
    convertRight (vs,s) = productVariant vs (variantFromValue s)

convertEither2 ::
    ( --Productable (V s1x)   (V '[s2])
    -- , Productable (V '[s1]) (V s2x)
    KnownNat (Length s2x)
    , KnownNat (Length (Product '[s1] s2x))
    , KnownNat (Length (Product s1x '[s2]))
    , 'True ~ IsSubset (Product s1x '[s2]) (Concat (Product s1x '[s2]) (Product '[s1] s2x))
    , 'True ~ IsSubset (Product '[s1] s2x) (Concat (Product s1x '[s2]) (Product '[s1] s2x))
    , LiftVariant (Product s1x '[s2]) (Concat (Product s1x '[s2]) (Product '[s1] s2x))
    , LiftVariant (Product '[s1] s2x) (Concat (Product s1x '[s2]) (Product '[s1] s2x))
    ) => Either (s1,V s2x) (V s1x, s2)
    -> V (Concat (Product s1x '[s2]) (Product '[s1] s2x))
convertEither2 (Left (s1, v_s2x)) =   liftVariant (productVariant (variantFromValue s1) v_s2x)
convertEither2 (Right (v_s1x, s2)) =  liftVariant (productVariant v_s1x (variantFromValue s2))

productWithValue1 :: (Variant s1x ,s2) -> Variant (Product s1x '[s2])
productWithValue1 (v_s1x, s2) =
  productVariant v_s1x (variantFromValue s2)

productWithValue2 :: KnownNat (Length s2x) =>
                     (s1, Variant s2x) -> Variant (Product '[s1] s2x)
productWithValue2 (s1, v_s2x) =
  productVariant (variantFromValue s1) v_s2x

combineUnionFlip ::
  ( Liftable xs (Union ys xs)
  , Liftable ys (Union ys xs)
  ) => Either (Variant ys) (Variant xs) -> Variant (Union ys xs)
{-# INLINE combineUnionFlip #-}
combineUnionFlip = \case
  Right xs -> liftVariant xs
  Left  ys -> liftVariant ys

(.~|><) ::
  ( Liftable xs zs
  , Liftable ys zs
  , zs ~ Union ys xs
  , Monad m
  ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (.~|><) #-}
(.~|><) v f = makeFlowOp selectFirst (applyF f) combineUnionFlip v

infixl 0 .~|><

-- | Lift an operation on a Variant into an operation on a flow
liftm :: Monad m => (Variant x -> a -> m b) -> Flow m x -> a -> m b
{-# INLINE liftm #-}
liftm op x a = do
   x' <- x
   op x' a

-- | Take the first output, fusion the result
(>.~|><) ::
  ( Liftable xs zs
  , Liftable ys zs
  , zs ~ Union ys xs
  , Monad m
  ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (>.~|><) #-}
(>.~|><) = liftm (.~|><)

-- | Bind two flows in a monadish way (error types union)
flowBindToTail :: forall xs ys zs m x.
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union ys xs
   , Monad m
   ) => Flow m (x ': ys) -> (x -> Flow m xs) -> Flow m zs
{-# INLINE flowBindToTail #-}
flowBindToTail = (>.~|><)

bindPopableToHead :: forall x xs ys zs m.
   ( Monad m
   , Popable x xs
   , zs ~ Union ys (Filter x xs)
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE bindPopableToHead #-}
bindPopableToHead flow_xs f = do
    v_xs <- flow_xs
    case popVariant v_xs of
        Right x -> liftVariant <$> f x
        Left ys -> liftVariant <$> return ys

bindPopableToTail :: forall x xs ys zs m.
  ( Monad m
  , Popable x xs
  , zs ~ Union (Filter x xs) ys
  , Liftable (Filter x xs) zs
  , Liftable ys zs
  ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE bindPopableToTail #-}
bindPopableToTail flow_xs f = do
    v_xs <- flow_xs
    case popVariant v_xs of
        Right x -> liftVariant <$> f x
        Left ys -> liftVariant <$> return ys

-----------------

-----------------
convertTuple :: (V s1x, s2) -> V (Product s1x '[s2])
convertTuple (v_s1x, s2) = productVariant v_s1x (variantFromValue s2)

caseVar :: ContVariant xs =>
           Variant xs -> Haskus.Utils.Tuple.ListToTuple (AddR xs r) -> r
caseVar v funcs = variantToCont v >::> funcs

bindFlow :: (ContVariant xs, Monad m) =>
           Variant xs -> Haskus.Utils.Tuple.ListToTuple (AddR xs (Flow m xs1)) -> Flow m xs1
bindFlow v funcs = variantToCont v >::> funcs

splitVar :: 
  ( bs ~ FilterList as cs
  , VariantSplitter as bs cs
  , ListNotEmpty as
  ) => Variant cs -> Proxy as -> Proxy bs -> Either (Variant as) (Variant bs)
splitVar  = splitVar'

proxyOfFilter :: (bs ~ FilterList as cs) => Variant cs -> Proxy as -> Proxy bs
proxyOfFilter v_cs p_as = Proxy

splitVar1 :: 
  ( bs ~ FilterList as cs
  , VariantSplitter as bs cs
  , ListNotEmpty as
  ) => Variant cs -> Proxy (as :: [*]) -> Either (Variant as) (Variant bs)
splitVar1 = splitVar1'

splitVar1' :: 
  ( bs ~ FilterList as cs
  , VariantSplitter as bs cs
  ) => Variant cs -> Proxy (as :: [*]) -> Either (Variant as) (Variant bs)
splitVar1' v_cs p_a = splitVar' v_cs p_a (proxyOfFilter v_cs p_a)

splitVar' :: 
  ( bs ~ FilterList as cs
  , VariantSplitter as bs cs
  ) => Variant cs -> Proxy as -> Proxy bs -> Either (Variant as) (Variant bs)
splitVar' v_cs px py = 
  splitVariant v_cs px py
  
type family ListEq (xs :: [*]) (ys :: [*]) :: Bool where
  ListEq xs xs = 'True
  ListEq xs ys = 'False

class CopyVar xs ys (listEq :: Bool) where
  copyVar :: V xs -> Proxy listEq -> V ys
 
instance (xs ~ ys) => CopyVar xs ys 'True where
  copyVar v_xs _ = v_xs

instance CopyVar xs ys 'False where
  copyVar v_xs _ = undefined

class SplitFastOr as bs cs (asEq :: Bool) (bsEq :: Bool) where
  splitFastOr :: Variant cs -> Proxy as -> Proxy bs -> Proxy asEq -> Proxy bsEq -> Either (Variant as) (Variant bs)
   
instance (VariantSplitter' as bs (c:cs)  (IsMemberOfList c as)) => SplitFastOr as bs (c:cs) 'False 'False where
  splitFastOr v_cs px py _ _ = 
    let pxBool :: Proxy (IsMemberOfList c as)
        pxBool = Proxy
    in splitVariant' v_cs px py pxBool 

-- both True-s are impossible    
instance (VariantSplitter' as bs (c:cs)  (IsMemberOfList c as)) => SplitFastOr as bs (c:cs) 'True 'True where
  splitFastOr v_cs px py _ _ = 
    let pxBool :: Proxy (IsMemberOfList c as)
        pxBool = Proxy
    in splitVariant' v_cs px py pxBool 

instance (as ~ cs) => SplitFastOr as bs cs 'True 'False where
  splitFastOr v_cs px py _ _ = Left v_cs 
    
instance (bs ~ cs) => SplitFastOr as bs cs 'False 'True where
  splitFastOr v_cs px py _ _ = Right v_cs 
          
class VariantSplitter as bs cs where
  splitVariant :: Variant cs -> Proxy as -> Proxy bs -> Either (Variant as) (Variant bs)

instance VariantSplitter as bs '[]  where
  splitVariant v_cs _ _  = undefined    

instance (SplitFastOr as bs (c : cs) (ListEq (c : cs) as) (ListEq (c : cs) bs)) => VariantSplitter as bs (c : cs) where
  splitVariant v_cs px py = splitFastOr v_cs px py (Proxy @(ListEq (c : cs) as)) (Proxy @(ListEq (c : cs) bs))
    
class VariantSplitter' as bs cs (fl ::Bool) where
  splitVariant' :: Variant cs -> Proxy as -> Proxy bs -> Proxy fl -> Either (Variant as) (Variant bs)

instance VariantSplitter' as bs '[] (fl ::Bool) where
  splitVariant' v_cs _ _ _ = undefined

instance (IsSubset '[c] as ~ True, Liftable '[c] as, VariantSplitter as bs cx) => VariantSplitter' as bs (c:cx) 'True where
  splitVariant' v_cs px py _ =     
    case popVariantHead v_cs of
      Right x -> Left $ liftVariant (variantFromValue x)
      Left cs_tail -> splitVariant cs_tail px py

instance (IsSubset '[c] bs ~ True, Liftable '[c] bs, VariantSplitter as bs cx) => VariantSplitter' as bs (c:cx) 'False where
  splitVariant' v_cs px py _ =     
    case popVariantHead v_cs of
      Right x -> Right $ liftVariant (variantFromValue x)
      Left cs_tail -> splitVariant cs_tail px py

headToTailV ::
  ( IsSubset '[x] (Concat xs '[x]) ~ 'True
  , IsSubset xs (Concat xs '[x]) ~ 'True
  , LiftVariant xs (Concat xs '[x])
  , LiftVariant '[x] (Concat xs '[x])
  ) => V (x ': xs) -> V (Concat xs '[x])
headToTailV v_xs = 
  case popVariantHead v_xs of
    Right x -> liftVariant (variantFromValue x)
    Left v_filtered -> liftVariant v_filtered
      
      

getVarLength :: forall xs. KnownNat (Length xs) => Variant xs -> Int
getVarLength v_xs = natValue @(Length xs)
