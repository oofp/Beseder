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
        
--- Get
type family GottenByName name  st :: * where
  GottenByName name (St a name) = ByName name (St a name)
  GottenByName name (St a name, nextPart) = ByName name (St a name)
  GottenByName name (St a name1, nextPart) = (GottenByName name nextPart)

type family GetByNameDepth name  st :: Nat where
  GetByNameDepth name (St a name) = 0
  GetByNameDepth name (St a name, nextPart) = 0
  GetByNameDepth name (St a name1, nextPart) = GetByNameDepth name nextPart + 1

type family GetNested  st (n ::Nat) :: * where
  GetNested (St a name) 0 = ByName name (St a name)
  GetNested (St a name, nextPart) 0 = ByName name (St a name)
  GetNested (St a name, nextPart) n = GetNested  nextPart (n-1)

type family GottenByName2 name st :: * where
  GottenByName2 name st = GetNested st (GetByNameDepth name st)

{-
class GetByName name st where
   getByName :: Named name -> st -> GottenByName2 name st
instance GetByName name (St a name) where
  getByName _  = ByName
instance {-# OVERLAPS #-}  GetByName name (St a name, nextPart) where
  getByName _ (st,_) = ByName st
instance {-# OVERLAPS #-} (GetByName name nextPart) => GetByName name (St a name1, nextPart) where
  getByName named (st,nextPart) = getByName named nextPart
-}

{-
class GetByName name st where
  type GotByName name st
  getByName :: Named name -> st -> GotByName name st

instance GetByName name (St a name) where
  type GotByName name  (St a name)  = ByName name (St a name)
  getByName _ = ByName

instance GetByName name (St a name, St b name1) where
  type GotByName name (St a name, St b name1)  = ByName name (St a name)
  getByName _ (a,b) = ByName a

instance GetByName name (St a name1, St b name) where
  type GotByName name (St a name1, St b name)  = ByName name (St b name)
  getByName _ (a,b) = ByName b
-}

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
  getTypeByNameVar _named _v = undefined

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


--

type (:<>) t1 t2 = (t1,t2) 
infixr 1 :<>

type T4 = Int :<> Char :<> Bool :<> Text
type family Third t4 where
  Third (t1, (t2, (t3,t4))) = t3
