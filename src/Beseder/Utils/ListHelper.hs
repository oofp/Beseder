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
{-# LANGUAGE PolyKinds #-}

module Beseder.Utils.ListHelper where

import           Haskus.Utils.Types.List
import           Protolude

type family FilterList (xs :: [k]) (ys :: [k]) :: [k] where
  FilterList '[] ys = ys
  FilterList (x ': xtail) ys = FilterList xtail (Filter x ys)

-- TODO:: unify with Contains from Machine   
type family ListContains (x :: *) (xs :: [*]) :: Bool where
  ListContains x '[] = 'False
  ListContains x (x ': xs) = 'True
  ListContains x (y ': xs) = ListContains x xs
     
type family SelectList (xs :: [*]) (ys :: [*]) :: [*] where
  SelectList '[] ys = '[]
  SelectList (x ': xtail) ys = Reverse (SelectList' x (ListContains x ys) xtail ys '[])
    
type family SelectList' (x :: *) (isM :: Bool) (xtail :: [*]) (ys :: [*]) (sel :: [*]) :: [*] where
  SelectList' x 'True '[] ys sel = x ': sel
  SelectList' x 'False '[] ys sel = sel
  SelectList' x 'False (xnext ': xtail) ys sel = SelectList' xnext (ListContains xnext ys) xtail ys sel
  SelectList' x 'True (xnext ': xtail) ys sel = SelectList' xnext (ListContains xnext ys) xtail ys (x ': sel)
    
type family IsListEmpty (l :: [*]) :: Bool where
  IsListEmpty '[] = 'True
  IsListEmpty l =   'False    

type family ListEmpty ((l :: [*])) :: Constraint where
  ListEmpty l = IsListEmpty l ~ 'True  

type family ListNotEmpty ((l :: [*])) :: Constraint where
  ListNotEmpty l = IsListEmpty l ~ 'False  
 
type family IsMemberOfList (a :: *) (lst :: [*]) :: Bool where
  IsMemberOfList a '[] = 'False
  IsMemberOfList a (a ': xs) = 'True
  IsMemberOfList a (b ': xs) = IsMemberOfList a xs
      
type family IfEmpty  (lst :: [*]) (subs :: [*]) :: [*] where
  IfEmpty '[] subs = subs       
  IfEmpty xs _ = xs     

type family UnitIfEmpty (lst :: [*]) :: [*] where 
  UnitIfEmpty lst = IfEmpty lst '[()]    

type family ListsIntersect  (lst1 :: [k]) (lst2 :: [k]) :: Bool where   
  ListsIntersect lst1 lst2 = 'False  

type family IsSublist (bigList :: [k]) (smallList :: [k]) :: Bool where
  IsSublist bigList '[] = True
  IsSublist bigList (x ': xs) = IsSublist' bigList (IsMemberOfList x bigList) xs

type family IsSublist' (bigList :: [k]) (fl :: Bool) (smallList :: [k]) :: Bool where
  IsSublist' bigList False smallList = False
  IsSublist' bigList True smallList  = IsSublist bigList smallList
    