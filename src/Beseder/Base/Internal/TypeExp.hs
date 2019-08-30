{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.TypeExp where

import Data.Kind
import Protolude

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data TFilterList :: (a -> Exp Bool) -> [*] -> Exp [*]

type instance Eval (TFilterList f '[]) = '[]

type instance Eval (TFilterList f (a ': as)) 
    = PrependIfTrue (Eval (f a)) a (Eval (TFilterList f as))

type family PrependIfTrue (f ::Bool) a (as :: [*]) where
    PrependIfTrue 'False a as = as         
    PrependIfTrue 'True a as = a ': as         

