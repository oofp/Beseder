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

module Beseder.Base.Internal.SplitOps where

import           Protolude                    hiding (Product, handle)
import           Haskus.Utils.Flow
import           Haskus.Utils.Types.List
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.Classes
import           Beseder.Utils.ListHelper
import           Beseder.Utils.Lst

class ListSplitter sp (xs :: [*]) where
  type ListSplitterRes sp xs :: [*]
  doSplit :: sp -> Lst xs -> Lst (ListSplitterRes sp xs)

type family ListSplitterReminder sp (xs :: [*]) :: [*] where
  ListSplitterReminder sp xs = FilterList (ListSplitterRes sp xs) xs

type family ListSplitterRes2 sp (xs :: [*]) :: ([*],[*]) where
  ListSplitterRes2 sp xs = '(ListSplitterRes sp xs, FilterList (ListSplitterRes sp xs) xs) 

data NoSplitter = NoSplitter 
instance ListSplitter NoSplitter xs where
  type ListSplitterRes NoSplitter xs = xs
  doSplit _ _ = Lst
instance GetInstance NoSplitter where
  getInstance = NoSplitter

type All = NoSplitter

type family SelectBy (par :: k) (xs :: [*]) :: [*] where
  SelectBy (n :: Symbol) xs = FilterByNestedName n xs
  SelectBy (t :: *) xs = FilterByNestedType t xs
  SelectBy (fltr :: [*]) xs = SelectList fltr xs  
  SelectBy (s :: StateTransKind) xs = FilterByStateTransKind s xs  
  SelectBy (predicate :: a -> Exp Bool) xs =  Eval (FilterListOfTuples predicate xs)

data By (by :: k) = By
instance ListSplitter (By k) xs where
  type ListSplitterRes (By k) xs = SelectBy k xs
  doSplit _ _ = Lst
instance GetInstance (By k) where
  getInstance = By
  
data Not (sp :: *) = Not sp 
instance (ListSplitter sp xs) => ListSplitter (Not sp) xs where
  type ListSplitterRes (Not sp) xs = FilterList (ListSplitterRes sp xs) xs
  doSplit _ _ = Lst
instance (GetInstance sp) => GetInstance (Not sp) where
  getInstance = Not (getInstance)
 
type Dynamics = By 'Dynamic
type Statics = By 'Static
  
data a :&& b = a :&& b
infixr 7 :&&
  
instance (ListSplitter sp1 xs, ListSplitter sp2 (ListSplitterRes sp1 xs)) => ListSplitter (sp1 :&& sp2) xs where
  type ListSplitterRes (sp1 :&& sp2) xs = ListSplitterRes sp2 (ListSplitterRes sp1 xs)
  doSplit _ _ = Lst
instance (GetInstance sp1, GetInstance sp2) => GetInstance (sp1 :&& sp2) where
  getInstance = getInstance :&& getInstance
    
data a :|| b = a :|| b
infixr 7 :||
  
instance (ListSplitter sp1 xs, ListSplitter sp2 xs, xs12 ~ Union (ListSplitterRes sp1 xs) (ListSplitterRes sp2 xs), Liftable (ListSplitterRes sp1 xs) xs12, Liftable (ListSplitterRes sp2 xs) xs12) => ListSplitter (sp1 :|| sp2) xs where
  type ListSplitterRes (sp1 :|| sp2) xs = Union (ListSplitterRes sp1 xs) (ListSplitterRes sp2 xs)
  doSplit _ _ = Lst
instance (GetInstance sp1, GetInstance sp2) => GetInstance (sp1 :|| sp2) where
  getInstance = getInstance :|| getInstance
  

type (:?) (name :: Symbol) (pred :: a -> Exp Bool) = By (PredWithName (Named name) pred) 
infixr 8 :?

predWithName :: forall pred name. Named name -> By (PredWithName (Named name) pred) 
predWithName named = By
