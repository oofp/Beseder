{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ConstraintKinds           #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Beseder.Base.Internal.TupleHelper where

import           GHC.TypeLits
import           Beseder.Base.Internal.Core
import           Protolude hiding (TypeError)
import           GHC.Exts
import           Beseder.Base.Internal.Named
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.TypeExp
import           Data.Function (id)
import           Prelude (error)
import qualified Prelude as SafeUndef (undefined) 

type family TargetIndex name a :: Nat where
  TargetIndex name a = TargetIndex' name a 0

type family TargetIndex' name  a (n ::Nat) :: Nat where
  TargetIndex' name (St _ name) n = n
  TargetIndex' name (St _ name, _) n = n
  TargetIndex' name (St _ name1, nextPart) n = TargetIndex' name nextPart (n+1)

type family TypeByIndex  (n ::Nat) a where
  TypeByIndex 0 (St x name) = (St x name)
  TypeByIndex 0 (st_x,_) = st_x
  TypeByIndex n (_,nextPart) = TypeByIndex (n-1) nextPart

type family TypeByName name a where
  TypeByName name a = TypeByIndex (TargetIndex name a) a

type family GetResByName name a where
  GetResByName name (St a name) = St a name
  GetResByName name (St a name, nextPart) = St a name
  GetResByName name (St a name1, nextPart) = GetResByName name nextPart --recursive
  GetResByName name (St a name1) = TypeError ('Text "Resource " :<>: 'Text name :<>: 'Text " not found at " 
                                              :$$: 'ShowType (St a name))
  GetResByName name () = TypeError ( 'Text "Resource " :<>: 'Text name :<>: 'Text " not found")

type family TargetByName name  a :: * where
  TargetByName name (St a name) = St a name
  TargetByName name (St a name, nextPart) = (St a name, nextPart)
  TargetByName name (St a name1, nextPart) = RT (St a name1) (TargetByName name nextPart) --recursive
  TargetByName name ((St a name, St b name1), St c name2) = (St a name, (St b name1, St c name2))
  TargetByName name ((St a name1, St b name), St c name2) = RT (St a name1) (St b name, St c name2)
  TargetByName name ((St a name1, St b name2), St c name) = RT (St a name1) (RT (St b name2) (St c name))
  TargetByName name (St a name1) = TypeError ('Text "Resource " :<>: 'Text name :<>: 'Text " not found at " 
                                              :$$: 'ShowType (St a name))
  TargetByName name () = TypeError ( 'Text "Resource " :<>: 'Text name :<>: 'Text " not found")

class TT t1 t2 where
  tt :: t1 -> t2

instance {-# OVERLAPS #-} TT a a where
  tt a = a

instance {-# OVERLAPS #-} TT (a, b) (RT a b) where
  tt = coerce -- (a,b) = RT (a, b)

instance {-# OVERLAPS #-} TT (a, b) (a,b) where
  tt = id -- (a,b) = (a, b)

instance {-# OVERLAPS #-} TT (a, (b, c)) (a,(b,c)) where
  tt = id -- (a,(b,c)) = (a, (b,c))

instance {-# OVERLAPS #-} TT (a, (b, c)) (RT a (b,c)) where
  tt = coerce -- (a,(b,c)) = RT (a, (b,c))

instance {-# OVERLAPS #-} TT (a, (b, c)) (RT a (RT b c)) where
  tt = coerce -- (a,(b,c)) = RT (a, RT (b,c))

instance {-# OVERLAPS #-} TT ((a, b), c) (a,(b,c)) where
  tt ((a, b), c) = (a, (b,c))

instance {-# OVERLAPS #-} TT ((a, b), c) (RT a (b,c)) where
  tt ((a, b), c) = RT (a, (b,c))

instance {-# OVERLAPS #-} TT ((a, b), c) (RT a (RT b c)) where
  tt ((a, b), c) = RT (a, RT (b,c))

instance {-# OVERLAPS #-} TT (a, (b, (c,d))) (a,(b,(c,d))) where
  tt = id -- (a,(b,(c,d))) = (a, (b,(c,d)))

instance {-# OVERLAPS #-} TT (a, (b, (c,d))) (RT a (b,(c,d))) where
  tt = coerce -- (a,(b,(c,d))) = RT (a, (b,(c,d)))

instance {-# OVERLAPS #-} TT (a, (b, (c,d))) (RT a (RT b (c,d))) where
  tt = coerce -- (a,(b,(c,d))) = RT (a, RT (b,(c,d)))

instance {-# OVERLAPS #-} TT (a, (b, (c,d))) (RT a (RT b (RT c d))) where
  tt = coerce -- (a,(b,(c,d))) = RT (a, RT (b, RT (c,d)))

instance {-# OVERLAPS #-} TT (a, (b, (c,(d,e)))) (RT a (RT b (RT c (RT d e)))) where
  tt = coerce -- (a,(b,(c,(d,e)))) = RT (a, RT (b, RT (c, (RT (d,e)))))
  
instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,f))))) (RT a (RT b (RT c (RT d (RT e f))))) where
    tt = coerce -- (a,(b,(c,(d,(e,f))))) = RT (a, RT (b, RT (c, (RT (d, (RT (e,f)))))))
    
instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,g)))))) (RT a (RT b (RT c (RT d (RT e (RT f g)))))) where
  tt = coerce -- (a,(b,(c,(d,(e,f))))) = RT (a, RT (b, RT (c, (RT (d, (RT (e,f)))))))

instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,h))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g h))))))) where
  tt = coerce -- (a,(b,(c,(d,(e,f))))) = RT (a, RT (b, RT (c, (RT (d, (RT (e,f)))))))
    
instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,(h,i)))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g (RT h i )))))))) where
  tt = coerce 

instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,(h,(i,j))))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g (RT h (RT i j ))))))))) where
  tt = coerce 
        
instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,(h,(i,(j,k)))))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g (RT h (RT i (RT j k)))))))))) where
  tt = coerce 
      
instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,(h,(i,(j,(k,l))))))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g (RT h (RT i (RT j (RT k  l)))))))))))  where
  tt = coerce 

instance {-# OVERLAPS #-} TT (a, (b, (c,(d,(e,(f,(g,(h,(i,(j,(k,(l,m)))))))))))) (RT a (RT b (RT c (RT d (RT e (RT f (RT g (RT h (RT i (RT j (RT k (RT l m)))))))))))) where
  tt = coerce 
    
newtype ByName name a  = ByName a

transformForReq :: TT a (TargetByName name a) => ByName name a -> TargetByName name a
transformForReq (ByName a)  = tt a

byName :: Named name -> a -> ByName name a
byName _ = ByName

data ReqByName name req a =  ReqByName req a
byNameR :: ReqByName name req a -> ByName name a
byNameR (ReqByName _ a) = ByName a

class GetTarget a b where
  getTarget :: a -> b

instance GetTarget () ()  where
  getTarget () = ()

instance GetTarget (St a name) (St a name)  where
  getTarget a = a

instance GetTarget (St a name,b) (St a name) where
  getTarget (a,_) = a

instance (GetTarget b c) => GetTarget (RT a b) c where
  getTarget (RT (_,b)) = getTarget b


getByName :: (TT a (TargetByName name a), GetTarget (TargetByName name a) (TypeByName name a))  => Named name -> a -> TypeByName name a
getByName nm a = getTarget (transformForReq (byName nm a))

class OrderByName (names :: [Symbol]) a  where
  type OrderByNameRes names a
  orderByName :: Proxy names -> a -> OrderByNameRes names a

instance 
  ( a ~ (t1,t2)
  , TT a (TargetByName name a)
  , GetTarget (TargetByName name a) (TypeByName name a)
  ) => OrderByName '[name] (t1,t2) where
    type OrderByNameRes '[name] (t1,t2) = TypeByName name (t1,t2)
    orderByName _  = getByName (Named @name) -- a   

instance 
  ( a ~ (t1,t2)
  , TypeByName name a ~ b
  , TT a (TargetByName name a)
  , GetTarget (TargetByName name a) (TypeByName name a)
  , OrderByName (name1 ': ns) a 
  ) => OrderByName (name ': name1 ': ns) (t1,t2) where
    type OrderByNameRes (name ': name1 ': ns) (t1,t2) = (TypeByName name (t1,t2), OrderByNameRes (name1 ': ns) (t1,t2)) 
    orderByName _ a = (getByName (Named @name) a, orderByName (Proxy @(name1 ': ns)) a)   

type family ListOfVar (v :: *) :: [*] where
  ListOfVar (V xs) = xs

instance 
  ( OrderByName' names x
  , OrderByName names (V xs)
  , a ~  OrderByNameRes names x
  , V ys ~ OrderByNameRes names (V xs)
  , res ~ Union '[a] ys
  , Liftable '[a] res
  , Liftable ys res
  ) =>  OrderByName names (V (x ': xs)) where
    type OrderByNameRes names (V (x ': xs)) = V (Union '[OrderByNameRes names x]  (ListOfVar (OrderByNameRes names (V xs)))) 
    orderByName names v_x_xs =  
      case popVariantHead v_x_xs of
        Right x -> liftVariant (variantFromValue (orderByName names x))
        Left v_xs -> liftVariant (orderByName names v_xs)

instance OrderByName names (V '[]) where
  type OrderByNameRes names (V '[]) = (V '[])
  orderByName _names v = v  

type OrderByName' names t = (OrderByName names t, Nub names ~ names, TupleLength t ~ Length names)

--- Append
class AppendToTuple x y where
  type AppendToTupleResult x y
  appendToTuple :: x -> y -> AppendToTupleResult x y

instance AppendToTuple ()  a where
  type AppendToTupleResult () a = a
  appendToTuple () a = a
  
instance AppendToTuple (St a name) b where
  type AppendToTupleResult (St a name) b = (St a name, b)
  appendToTuple x y = (x,y)

instance AppendToTuple (St a name, St b name1) c where
  type AppendToTupleResult (St a name, St b name1) c = (St a name, (St b name1, c))
  appendToTuple (x, y) z = (x, (y,z))


instance AppendToTuple (St a name, (St b name1, St c name2)) d where
  type AppendToTupleResult (St a name, (St b name1, St c name2)) d = (St a name, (St b name1, (St c name2, d)))
  appendToTuple (a,(b ,c)) d = (a, (b, (c,d)))

instance AppendToTuple (St a name, (St b name1, (St c name2, St d name3))) e where
  type AppendToTupleResult (St a name, (St b name1, (St c name2, St d name3))) e =
         (St a name, (St b name1, (St c name2, (St d name3, e))))
  appendToTuple (a,(b ,(c,d))) e = (a, (b, (c,(d,e))))

instance AppendToTuple (St a name, (St b name1, (St c name2, (St d name3, St e name4)))) f where
  type AppendToTupleResult (St a name, (St b name1, (St c name2, (St d name3, St e name4)))) f =
          (St a name, (St b name1, (St c name2, (St d name3, (St e name4 ,f)))))
  appendToTuple (a,(b ,(c, (d,e)))) f = (a, (b, (c,(d,(e,f)))))


type family AppendToTupleList xs a  where
  AppendToTupleList '[] a = '[]
  AppendToTupleList '[x] a = '[AppendToTupleResult x a]
  AppendToTupleList (x ': xs) a = (AppendToTupleResult x a) ': (AppendToTupleList xs a)
  

instance 
  ( AppendToTuple x a
  , AppendToTuple (V xs) a
  , res ~ Union '[AppendToTupleResult x a] (AppendToTupleList xs a)
  , Liftable '[AppendToTupleResult x a] res
  , Liftable (AppendToTupleList xs a) res
  , AppendToTupleResult (V xs) a ~ V (AppendToTupleList xs a)
  ) =>  AppendToTuple (V (x ': xs)) a where
  type AppendToTupleResult (V (x ': xs)) a = V (Union '[AppendToTupleResult x a] (AppendToTupleList xs a))
  appendToTuple v_x_xs a =  case popVariantHead v_x_xs of
    Right x -> liftVariant (variantFromValue (appendToTuple x a))
    Left v_xs -> liftVariant (appendToTuple v_xs a)

emptyVar :: V '[]
emptyVar = error "empty Variant; will never happen"

instance  AppendToTuple (V '[]) a where
  type AppendToTupleResult (V '[]) a = V '[]
  appendToTuple _v_empty _a =  emptyVar

type family TupleLength t :: Nat where
  TupleLength (t1,t2) = 1 + TupleLength t2
  TupleLength (St t name) = 1

--- Remove
type family RemovedByName name  a :: * where
  RemovedByName name (St a name) = ()
  RemovedByName name (St a name, nextPart) = nextPart
  RemovedByName name (St a name1, nextPart) = (St a name1, RemovedByName name nextPart)

class Remove t1 t2 where
  remove :: t1 -> t2

instance Remove a () where
  remove _a = ()

instance Remove (a ,b) a where
  remove (a, _b) = a

instance Remove (a ,b) b where
  remove (_a, b) = b


instance Remove (a ,(b, c)) (a,b) where
  remove (a, (b, _c)) = (a,b)

instance Remove (a ,(b, c)) (a,c) where
  remove (a, (_b, c)) = (a,c)

instance Remove (a ,(b, c)) (b,c) where
  remove (_a, (b, c)) = (b,c)

removeByName :: Remove a  (RemovedByName name a) => ByName name a -> RemovedByName name a
removeByName (ByName a)  = remove a

type family IsTypeNested (t :: *) (extType :: k) :: Bool where
  IsTypeNested (St st name) (St st name) = 'True
  IsTypeNested (St st name) (St st name,_) = 'True
  IsTypeNested (St st name) (_,nextPart) = IsTypeNested (St st name) nextPart
  IsTypeNested (St st name) _ = 'False

type family IsNameNested (nm :: Symbol) (extType :: k) :: Bool where
  IsNameNested name (St st name) =   'True
  IsNameNested name (St st name,_) = 'True
  IsNameNested name (_,nextPart) = IsNameNested name nextPart
  IsNameNested name _ = 'False
    
type family IsTypeUnique (nm :: Symbol) (extType :: k) :: Constraint where
  IsTypeUnique nm extType = IsTypeUnique' nm extType (IsNameNested nm  extType) 
  
type family IsTypeUnique' (nm :: Symbol) (extType :: k) (flNested :: Bool) :: Constraint where
    IsTypeUnique' nm extType False = ()
    IsTypeUnique' nm extType True = 
      TypeError 
      ( 'Text "Resource "
        ':<>: 'Text nm :<>: 'Text "' already exists at '"
        ':<>: 'ShowType extType ':<>: 'Text "'")


{-  
type family StateTags (t :: *) :: [Symbol]
type instance StateTags a = '[]
--type instance StateTags (St Text name) = '["Demo"]
-}

type family ByNestedType (t :: *) (lst :: [*]) :: * where
  ByNestedType t (a ': lst) = ByNestedType' t a lst (IsTypeNested t a) 

type family ByNestedType' (t :: *) (a :: *) (lst :: [*]) (fl :: Bool) :: * where
  ByNestedType' t a lst 'True = a
  ByNestedType' t _ lst 'False = ByNestedType t lst 
  
type family FilterByNestedType (t :: *) (lst :: [*]) :: [*] where
  FilterByNestedType t '[] = '[]
  FilterByNestedType t (a ': lst) = FilterByNestedType' t a lst (IsTypeNested t a)      

type family FilterByNestedType' (t :: *) (a :: *) (lst :: [*]) (fl :: Bool) :: [*] where
  FilterByNestedType' t a lst 'True = a ': (FilterByNestedType t lst)
  FilterByNestedType' t a lst 'False = FilterByNestedType t lst

type family FilterByNestedName (nm :: Symbol) (lst :: [*]) :: [*] where
  FilterByNestedName nm '[] = '[]
  FilterByNestedName nm (a ': lst) = FilterByNestedName' nm a lst (IsNameNested nm a)      

type family FilterByNestedName' (nm :: Symbol) (a :: *) (lst :: [*]) (fl :: Bool) :: [*] where
  FilterByNestedName' nm a lst 'True = a ': (FilterByNestedName nm lst)
  FilterByNestedName' nm a lst 'False = FilterByNestedName nm lst

type family FilterByStateTransKind (s :: StateTransKind) (lst :: [*]) :: [*] where
  FilterByStateTransKind s '[] = '[]
  FilterByStateTransKind s (a ': lst) = FilterByStateTransKind' s a lst (StateTrans a)       

type family FilterByStateTransKind' (s :: StateTransKind) (a :: *) (lst :: [*]) (a_s ::StateTransKind) :: [*] where
  FilterByStateTransKind' 'Static a lst 'Static = a ': (FilterByStateTransKind 'Static lst)
  FilterByStateTransKind' 'Dynamic a lst 'Dynamic = a ': (FilterByStateTransKind 'Dynamic lst)
  FilterByStateTransKind' 'Dynamic a lst 'Static = FilterByStateTransKind 'Dynamic lst
  FilterByStateTransKind' 'Static a lst 'Dynamic = FilterByStateTransKind 'Static lst
    
data TMatchFound :: (a -> Exp Bool) -> Type -> Exp Bool
type instance Eval (TMatchFound f ()) = Eval (f ())  
type instance Eval (TMatchFound f (St a name)) = Eval (f (St a name))  
type instance Eval (TMatchFound f (a, b)) = TMatchFoundInTuple (Eval (f a)) b f

type family TMatchFoundInTuple (fl :: Bool) b (f :: (a -> Exp Bool)) :: Bool where
  TMatchFoundInTuple 'True _ _ = 'True
  TMatchFoundInTuple 'False b f = Eval (TMatchFound f b)
  
data FilterListOfTuples :: (a -> Exp Bool) -> [*] -> Exp [*]
type instance Eval (FilterListOfTuples f '[]) = '[] 
type instance Eval (FilterListOfTuples f (a ': as)) = PrependIfTrue (Eval (TMatchFound f a)) a (Eval (FilterListOfTuples f as)) 

data AndPred :: (a -> Exp Bool) -> (a -> Exp Bool) -> a -> Exp Bool
type instance Eval (AndPred f1 f2 a) = AndBoolean (Eval (f1 a)) (Eval (f2 a))

type family AndBoolean (fl1 :: Bool) (fl2 :: Bool) where
  AndBoolean 'True 'True = 'True
  AndBoolean _ _ = 'False

data PredWithName :: named -> (a -> Exp Bool) -> a -> Exp Bool
type instance Eval (PredWithName named f a) = AndBoolean (Eval (f a)) (IsNameMatching named a)

type family IsNameMatching named st :: Bool where
  IsNameMatching (Named name) (St a name) = 'True
  IsNameMatching _ _ = False


type family IsTypeUniqueList (name :: Symbol) (xs :: [*]) :: Constraint where
  IsTypeUniqueList (name :: Symbol) '[] = ()
  IsTypeUniqueList  name (x ': xs) = (IsTypeUnique name x, IsTypeUniqueList name xs)


class GetTypeByNameVar (name :: Symbol) t xs where
  getTypeByNameVar :: Named name -> V xs -> t
  
instance GetTypeByNameVar name t '[] where
  getTypeByNameVar _named _v = SafeUndef.undefined 


instance (TT x (TargetByName name x), GetTarget (TargetByName name x) (TypeByName name x), TypeByName name x ~ t, GetTypeByNameVar name t xs) => GetTypeByNameVar name t (x ': xs) where
  getTypeByNameVar named v_x_xs = case popVariantHead v_x_xs of
      Right x -> getByName named x
      Left v_xs -> getTypeByNameVar named v_xs 


type family GetNames (t :: *) :: [Symbol] where
  GetNames () = '[]
  GetNames (St a name) = '[name]
  GetNames ((St a name), rest) = name ': GetNames rest

type family GetAllNames (lst :: [*]) :: [Symbol] where
  GetAllNames '[] = '[]    
  GetAllNames (x ': xs) = Union (GetNames x) (GetAllNames xs)    

proxyOfNames :: V xs -> Proxy (GetAllNames xs)
proxyOfNames _ = Proxy

--
