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
import           Protolude hiding (TypeError)
import           GHC.TypeLits

type family FilterList (xs :: [k]) (ys :: [k]) :: [k] where
  FilterList '[] ys = ys
  FilterList (x ': xtail) ys = FilterList xtail (Filter x ys)

-- TODO:: unify with Contains from Machine   
type family ListContains (x :: k) (xs :: [k]) :: Bool where
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
 
type family IsMemberOfList (a :: k) (lst :: [k]) :: Bool where
  IsMemberOfList a '[] = 'False
  IsMemberOfList a (a ': xs) = 'True
  IsMemberOfList a (b ': xs) = IsMemberOfList a xs
      
type family IfEmpty  (lst :: [*]) (subs :: [*]) :: [*] where
  IfEmpty '[] subs = subs       
  IfEmpty xs _ = xs     

type family UnitIfEmpty (lst :: [*]) :: [*] where 
  UnitIfEmpty lst = IfEmpty lst '[()]    

type family ListsIntersect  (lst1 :: [k]) (lst2 :: [k]) :: [k] where   
  ListsIntersect '[] lst2 = '[]
  ListsIntersect (x ': xs) lst2 =  ListsIntersect' (ListContains x lst2) x xs lst2

type family ListsIntersect' (fl :: Bool) (x :: k) (lst1 :: [k]) (lst2 :: [k]) :: [k] where   
  ListsIntersect 'False x lst1 lst2 = ListsIntersect lst1 lst2
  ListsIntersect 'True x lst1 lst2 = x ': (ListsIntersect lst1 lst2)

type family IsSublist (bigList :: [k]) (smallList :: [k]) :: Bool where
  IsSublist bigList '[] = True
  IsSublist bigList (x ': xs) = IsSublist' bigList (IsMemberOfList x bigList) xs

type family IsSublist' (bigList :: [k]) (fl :: Bool) (smallList :: [k]) :: Bool where
  IsSublist' bigList False smallList = False
  IsSublist' bigList True smallList  = IsSublist bigList smallList
    
type family FromSingletonList (xs :: [*]) :: * where
  FromSingletonList '[x] = x
  FromSingletonList _ = TypeError ('Text "Should have one element")

type family ListElem (n :: Nat) (xs :: [*]) :: * where
  ListElem n '[] = TypeError ('Text "no element found")
  ListElem 0 (x ': xs) = x
  ListElem n (x ': xs) = ListElem (n-1) xs
    

--Subtract '["1","2"] '["1"] :: [Symbol]
-- = '["2"]
-- Subtract '["1","2","5","3"] '["1","3"] :: [Symbol]
-- = '["2", "5"] 
type family Subtract (xs :: [k]) (ys :: [k]) :: [k] where
  Subtract '[] ys = '[]
  Subtract (x ': xs) ys = (Subtract' x (IsMemberOfList x ys) xs ys) 
 
type family Subtract' (x :: k) (isMember :: Bool) (xs :: [k]) (ys :: [k]) :: [k] where
  Subtract' x False xs ys = x ': (Subtract xs ys)
  Subtract' x True xs ys = Subtract xs ys

subtractLists :: Proxy xs -> Proxy ys -> Proxy (Subtract xs ys)
subtractLists _ _ = Proxy  

type family HeadOfList (xs :: [*]) :: * where
  HeadOfList xs = ListElem 0 xs
