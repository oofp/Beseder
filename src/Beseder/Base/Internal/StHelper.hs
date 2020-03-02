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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.StHelper 
  ( StList
  , StWrapList
  , StVar (..)
  , StWrapVar (..)
  , UnwrapContent
  , AreEq
  , IsContentEq
  , stFunc
  , SupportedRequests 
  , StReqs
  , StateTitle
  , ShowState
  , ShowStates
  , NextDataStates
  , StateDataTrans
  ) where

import            Protolude hiding (First, TypeError)
import            Haskus.Utils.Variant 
import            Beseder.Base.Internal.Core
import            Beseder.Base.Internal.Named
import            Beseder.Base.Internal.Flow
import            Beseder.Base.Internal.TypeExp
import            Beseder.Utils.ListHelper
import            Data.Coerce
import            Haskus.Utils.Types.List
import            GHC.TypeLits
import qualified Prelude as SafeUndef (undefined) 

stWithName :: stData -> Named name -> St stData name
stWithName stData _named = St stData
  
stFunc :: (a -> b) -> (St a name -> b)
stFunc f = f . coerce

type family StList name (dataList :: [*]) where
  StList name '[] = '[]
  StList name (stData ': dataLst) = (St stData name) ': (StList name dataLst)
  
class StVar xs name where
  asStVar :: Named name -> V xs -> V (StList name xs) 
  
instance StVar ('[]) name where
  asStVar _named _ = SafeUndef.undefined 


instance 
  ( StVar xs name 
  , Liftable '[St x name] (StList name (x ': xs))
  , Liftable (StList name xs) (StList name (x ': xs))) => StVar (x ': xs) name where
    asStVar named v_xs =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (stWithName x named))
          Left v_ys -> liftVariant $ asStVar named v_ys 

--
type family StWrapList (w :: * -> *)  name (dataList :: [*]) where
  StWrapList w name '[] = '[]
  StWrapList w name (stData ': dataLst) = (St (w stData) name) ': (StWrapList w name dataLst)
  
class StWrapVar (w :: * -> *) xs name where
  asStWrapVar :: (forall x. x -> w x) -> Named name -> V xs -> V (StWrapList w name xs) 
  
instance StWrapVar w ('[]) name where
  asStWrapVar _ _ _ = SafeUndef.undefined 


instance 
  ( StWrapVar w xs name 
  , Liftable '[St (w x) name] (StWrapList w name (x ': xs))
  , Liftable (StWrapList w name xs) (StWrapList w name (x ': xs))) => StWrapVar w (x ': xs) name where
    asStWrapVar wf named v_xs =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (stWithName (wf x) named))
          Left v_ys -> liftVariant $ asStWrapVar wf named v_ys 
  
  
type family UnwrapContent t 

type family AreEq a b :: Bool where
  AreEq a a = 'True
  AreEq a b = 'False

type family IsContentEq a b :: Bool where
  IsContentEq a b = AreEq (UnwrapContent a) b

type family GetResState st :: * where
  GetResState (St st name) = st

data SupportedRequests :: * -> (Exp [*]) 
type instance Eval (SupportedRequests ()) = '[]

type family NameRequests (reqs :: [*]) (name :: Symbol) :: [*] where
  NameRequests '[] name = '[]
  NameRequests (req ': rs) name = (NamedRequest req name) ': (NameRequests rs name)

type family StReqs (s :: *) :: [*] where
  StReqs () = '[]
  StReqs (St a name) = NameRequests (Eval (SupportedRequests (St a name))) name
  StReqs (a,b) = Union (StReqs a) (StReqs b)
  StReqs (V '[]) = '[]
  StReqs (V '[x]) = StReqs x
  StReqs (V (x ': xs)) = ListsIntersect (StReqs x) (StReqs (V xs))

  
type family StateTitle (st :: *) :: Symbol

-- data (:=) (n :: Symbol) (s :: Symbol)

type family ShowState (s :: *) :: Symbol where
  ShowState () = "()"
  ShowState (St s name) = AppendSymbol name (AppendSymbol " = " (StateTitle s))
  ShowState (St s name, moreStates) = AppendSymbol (ShowState (St s name)) (AppendSymbol " , " (ShowState moreStates))

type family ShowStates (sts :: [*]) :: [Symbol] where
  ShowStates '[] = '[]
  ShowStates (s ': moreStates) = (ShowState s) ': (ShowStates moreStates)

--
type family NextDataStates (st :: *) :: [*] where
  NextDataStates st = NextDataStates' (St st "") (StateTrans (St st "")) 

type family NextDataStates' (st :: *) (transKind :: StateTransKind) :: [*] where
  NextDataStates' stNamed 'Static = '[]
  NextDataStates' stNamed 'Dynamic = GetStatesData (NextStates stNamed)

type family GetStatesData (states :: [*])  :: [*] where
  GetStatesData '[] = '[]
  GetStatesData ((St st name) ': tail) = st ': (GetStatesData tail)
  GetStatesData (invState ': tail) = TypeError ('Text "Cannot GetStateData from " :<>: 'ShowType invState)

type family StateDataTrans stData :: StateTransKind where
   StateDataTrans stData = StateTrans (St stData "")
