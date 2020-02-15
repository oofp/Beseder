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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Beseder.Utils.VariantHelper where

import           Haskus.Utils.ContFlow
import           Haskus.Utils.Flow       hiding ((|>))
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Protolude               hiding (Product)
import           Beseder.Utils.ListHelper
import           Beseder.Base.Internal.Classes 
import           Data.Coerce
import           System.Random
import qualified Prelude as SafeUndef (undefined) 

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

proxyOfVar :: Variant cs -> Proxy cs
proxyOfVar _ = Proxy

proxyOfFilter :: (bs ~ FilterList as cs) => Variant cs -> Proxy as -> Proxy bs
proxyOfFilter _v_cs _p_as = Proxy

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
  copyVar _v_xs _ = SafeUndef.undefined 


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
  splitFastOr v_cs _px _py _ _ = Left v_cs 
    
instance (bs ~ cs) => SplitFastOr as bs cs 'False 'True where
  splitFastOr v_cs _px _py _ _ = Right v_cs 
          
class VariantSplitter as bs cs where
  splitVariant :: Variant cs -> Proxy as -> Proxy bs -> Either (Variant as) (Variant bs)

instance VariantSplitter as bs '[]  where
  splitVariant _v_cs _ _  = SafeUndef.undefined 

instance (SplitFastOr as bs (c : cs) (ListEq (c : cs) as) (ListEq (c : cs) bs)) => VariantSplitter as bs (c : cs) where
  splitVariant v_cs px py = splitFastOr v_cs px py (Proxy @(ListEq (c : cs) as)) (Proxy @(ListEq (c : cs) bs))
    
class VariantSplitter' as bs cs (fl ::Bool) where
  splitVariant' :: Variant cs -> Proxy as -> Proxy bs -> Proxy fl -> Either (Variant as) (Variant bs)

instance VariantSplitter' as bs '[] (fl ::Bool) where
  splitVariant' _v_cs _ _ _ = SafeUndef.undefined 


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
getVarLength _v_xs = natValue @(Length xs)

concatEither :: forall xs ys. KnownNat (Length xs) => Either (V xs) (V ys) -> V (Concat xs ys)
{-# INLINE concatEither #-}
concatEither (Left v_xs) = appendVariant @ys v_xs
concatEither (Right v_ys) = prependVariant @xs v_ys

concatRightLeft :: forall xs ys. KnownNat (Length ys) => Either (V xs) (V ys, ())  -> V (Concat ys xs)
{-# INLINE concatRightLeft #-}
concatRightLeft (Left v_xs) = prependVariant @ys v_xs
concatRightLeft (Right (v_ys,())) = appendVariant @xs v_ys

unionEither :: forall xs ys zs. 
  ( KnownNat (Length xs)
  , zs ~ (Concat xs ys)
  , Liftable zs (Nub zs)
  ) => Either (V xs) (V ys) -> V (Union xs ys)
{-# INLINE unionEither #-}
unionEither  = nubVariant . concatEither 

{-
class GetVarInstance (xs :: [*]) where
  getVarInstance :: Int -> V xs
  getVarSize :: Proxy xs -> Int

instance GetInstance x => GetVarInstance '[x] where
  getVarInstance _ = variantFromValue getInstance 
  getVarSize _ = 0

instance (Liftable (x1 ': xs) (x ': x1 ': xs), Liftable '[x] (x ': x1 ': xs), GetInstance x, GetVarInstance (x1 ': xs)) => GetVarInstance (x ': x1 ': xs) where
  getVarInstance indx = 
    let xsSize = getVarSize (Proxy @(x1 ':xs))
    in 
      if (xsSize == indx)
        then
          liftVariant $ variantFromValue (getInstance @x)
        else 
          let v_xs :: V (x1 ': xs)
              v_xs = getVarInstance indx 
          in liftVariant v_xs    
  getVarSize _px = getVarSize (Proxy @(x1 ':xs)) + 1  
-}

class GetVarInstance (xs :: [*]) where
  getVarInstance :: Int -> V xs

instance GetInstance x => GetVarInstance '[x] where
  getVarInstance _ = variantFromValue getInstance 
 
instance (Liftable (x1 ': xs) (x ': x1 ': xs), Liftable '[x] (x ': x1 ': xs), GetInstance x, GetVarInstance (x1 ': xs)) => GetVarInstance (x ': x1 ': xs) where
  getVarInstance 0 = liftVariant $ variantFromValue (getInstance @x)
  getVarInstance n = 
    let v_xs :: V (x1 ': xs)
        v_xs = getVarInstance (n-1) 
    in liftVariant v_xs    

class GetVarInstanceFrom (par :: *) (xs :: [*]) where
  getVarInstanceFrom :: par -> Int -> V xs

instance Coercible par x => GetVarInstanceFrom par '[x] where
  getVarInstanceFrom par _ = variantFromValue (coerce par) 
 
instance (Liftable (x1 ': xs) (x ': x1 ': xs), Liftable '[x] (x ': x1 ': xs), Coercible par x, GetVarInstanceFrom par (x1 ': xs)) => GetVarInstanceFrom par (x ': x1 ': xs) where
  getVarInstanceFrom par 0 = 
    let x :: x
        x = coerce par
    in liftVariant $ variantFromValue x
  getVarInstanceFrom par n = 
    let v_xs :: V (x1 ': xs)
        v_xs = getVarInstanceFrom par (n-1) 
    in liftVariant v_xs    

data Var xs = Var { unVar :: (V xs) }
data IndexedPar par = IndexedPar Int par

instance CreateFrom par x => CreateFrom (IndexedPar par) (Var '[x]) where
  createFrom (IndexedPar _ par) = Var $ variantFromValue (createFrom par) 
 
instance (Liftable (x1 ': xs) (x ': x1 ': xs), Liftable '[x] (x ': x1 ': xs), CreateFrom par x, CreateFrom (IndexedPar par) (Var (x1 ': xs))) => CreateFrom (IndexedPar par) (Var (x ': x1 ': xs)) where
  createFrom (IndexedPar 0 par) = 
    let x :: x
        x = createFrom par
        v_x = variantFromValue x
        v_xs = liftVariant v_x 
    in Var v_xs
  createFrom (IndexedPar n par) = 
    let var_xs :: Var (x1 ': xs)
        var_xs = createFrom (IndexedPar (n-1) par)  
    in Var $ (liftVariant (unVar var_xs))    

getVarSize :: forall xs. (KnownNat (Length xs)) => V xs -> Int 
getVarSize _v_xs = 
  let pxNat :: Proxy (Length xs)
      pxNat = Proxy 
  in fromIntegral $ natVal pxNat


getListSize :: forall xs. (KnownNat (Length xs)) => Proxy xs -> Int 
getListSize _px_xs = 
  let pxNat :: Proxy (Length xs)
      pxNat = Proxy 
  in fromIntegral $ natVal pxNat

getRandomVar :: forall xs m. (KnownNat (Length xs), GetVarInstance xs, MonadIO m) => m (V xs)
getRandomVar = do
  let varSize = getListSize (Proxy @xs)
  indx <- liftIO $ randomRIO (0, varSize)
  return $ getVarInstance indx

getRandomVarFrom :: forall par xs m. (KnownNat (Length xs), GetVarInstanceFrom par xs, MonadIO m) => par -> m (V xs)
getRandomVarFrom par = do
  let varSize = getListSize (Proxy @xs)
  indx <- liftIO $ randomRIO (0, varSize)
  return $ getVarInstanceFrom par indx


{-
type L4 = '[(),Text,Char,Int]          
instance GetInstance Char  where getInstance = '0'
instance GetInstance Text  where getInstance = ""
instance GetInstance Int  where getInstance = 0
--let getRVar :: IO (V L4); getRVar = getRandomVar
-}

class CoerceVar xs ys where
  coerceVar :: V xs -> V ys

instance CoerceVar '[] '[] where
  coerceVar v_xs = v_xs

instance (Coercible x y, CoerceVar xs ys, Liftable '[y] (y ': ys), Liftable ys (y ': ys)) => CoerceVar (x ': xs) (y ': ys) where
  coerceVar v_x_xs = case popVariantHead v_x_xs of
    Right x -> 
      let y :: y
          y = coerce x
      in liftVariant (variantFromValue y)
    Left v_xs -> 
      let v_ys :: V ys
          v_ys = coerceVar v_xs
      in liftVariant v_ys 


{-
newtype C = C Char deriving Show
newtype II = II Int deriving Show

vci,vci0 :: V [Char,Int] 
vci = variantFromEither (Right '0')
vci0 = variantFromEither (Left 5)

vci2, vci3 :: V [C,II]      
vci2 = coerceVar vci
vci3 = coerceVar vci0
-}
