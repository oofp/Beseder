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

module Beseder.Base.Internal.SplitFunc where

import           Protolude                    hiding (Product, handle)
import           Haskus.Utils.Flow
import           Haskus.Utils.Types.List
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.Classes
import           Beseder.Utils.ListHelper
import           Beseder.Utils.ListHelper
import           Beseder.Utils.Lst
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant

data NoSplitter :: * -> Exp Bool
data ByResName :: Symbol -> * -> Exp Bool
data ByRes :: * -> * -> Exp Bool
data ByList :: [*] -> * -> Exp Bool
data ByTransKind :: StateTransKind -> * -> Exp Bool

data (:&&) :: (* -> Exp Bool) -> (* -> Exp Bool) -> * -> Exp Bool
data (:||) :: (* -> Exp Bool) -> (* -> Exp Bool) -> * -> Exp Bool
data Not :: (* -> Exp Bool) -> * -> Exp Bool
infixr 7 :&&
infixr 7 :||

type instance Eval (NoSplitter t) = 'True
type instance Eval (ByResName name t) = IsNameNested name t
type instance Eval (ByRes res t) = IsTypeNested res t
type instance Eval (ByList lst t) = ListContains t lst
type instance Eval ((:&&) sp1 sp2 t) = AndRes (Eval (sp1 t)) (Eval (sp2 t))
type instance Eval ((:||) sp1 sp2 t) = OrRes (Eval (sp1 t)) (Eval (sp2 t))
type instance Eval (Not sp t) = NotRes (Eval (sp t))

type family AndRes (fl1 :: Bool) (fl2 :: Bool) :: Bool where
  AndRes False fl2 = False
  AndRes True fl2 = fl2

type family OrRes (fl1 :: Bool) (fl2 :: Bool) :: Bool where
  OrRes True fl2 = True
  OrRes False fl2 = fl2

type family NotRes (fl :: Bool) :: Bool where
  NotRes True = False
  NotRes False = True
    
type family SplitList (sp :: * -> Exp Bool) (xs :: [*]) :: ([*],[*]) where
  SplitList sp '[] = '(('[]) ,'[])
  SplitList sp (x ': xs) = PrependElement x (Eval(sp x)) (SplitList sp xs)

type family PrependElement (x :: *)  (fl :: Bool) (res :: ([*],[*])) :: ([*],[*]) where
  PrependElement x True '(r,l) = '(x ': r , l)  
  PrependElement x False '(r,l) = '(r , x ': l)  
  

type family  Var (res :: ([*],[*])) :: * where
  Var '(xs,ys) = Either (V ys) (V xs)

class VarFromVal (xs :: [*]) (ys :: [*]) (fl :: Bool) (x :: *) where
  varFromVal :: Proxy fl -> x -> Either (V ys) (V xs) 
instance (Liftable '[x] xs) => VarFromVal xs ys True x where
  varFromVal _px_true x = Right $ liftVariant $ variantFromValue x 
instance (Liftable '[x] ys) => VarFromVal xs ys False x where
  varFromVal _px_false x = Left $ liftVariant $ variantFromValue x 
  
class SplitVar (sp :: * -> Exp Bool) xs where
  splitVar :: Proxy sp -> Variant xs -> Var (SplitList sp xs)   

instance SplitVar sp '[] where
  splitVar px_sp _ = undefined  

--instance (VarFromValue SplitVar sp (x ': xs) where
--  splitVar px_sp xs =   case popVariantHead v_xs of
--    Right x -> liftVariant (variantFromValue x)
--    Left v_xs -> splitVar xs
    