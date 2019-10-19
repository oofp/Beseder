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
  ) where

import            Protolude hiding (First)
import            Haskus.Utils.Variant 
import            Beseder.Base.Common

stWithName :: stData -> Named name -> St stData name
stWithName stData _named = St stData
  
type family StList name (dataList :: [*]) where
  StList name '[] = '[]
  StList name (stData ': dataLst) = (St stData name) ': (StList name dataLst)
  
class StVar xs name where
  asStVar :: Named name -> V xs -> V (StList name xs) 
  
instance StVar ('[]) name where
  asStVar _named _ = undefined

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
  asStWrapVar _ _ _ = undefined

instance 
  ( StWrapVar w xs name 
  , Liftable '[St (w x) name] (StWrapList w name (x ': xs))
  , Liftable (StWrapList w name xs) (StWrapList w name (x ': xs))) => StWrapVar w (x ': xs) name where
    asStWrapVar wf named v_xs =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (stWithName (wf x) named))
          Left v_ys -> liftVariant $ asStWrapVar wf named v_ys 
  
