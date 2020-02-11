{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RankNTypes                #-}

module Beseder.Base.Internal.NamedVar 
  ( toVarOfSt
  , ListOfNamed
  , ToVarOfNamed 
  ) 
  where

import           Protolude
import           Beseder.Base.Internal.Named
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core (St (..))
import qualified Prelude as SafeUndef (undefined) 

type family ListOfNamed nt name (dataList :: [*]) where
  ListOfNamed nt name '[] = '[]
  ListOfNamed nt name (stData ': dataLst) = nt stData name ': (ListOfNamed nt name dataLst)

class ToVarOfNamed nt xs name where
  toVarOfNamed :: Named name -> V xs -> (forall x. x -> Named name -> nt x name) -> V (ListOfNamed nt name xs) 

instance ToVarOfNamed nt  ('[]) name where
  toVarOfNamed _named _ _ = SafeUndef.undefined
  
instance 
  ( ToVarOfNamed nt xs name 
  , Liftable '[nt x name] (ListOfNamed nt name (x ': xs))
  , Liftable (ListOfNamed nt name xs) (ListOfNamed nt name (x ': xs))) => ToVarOfNamed nt (x ': xs) name where
    toVarOfNamed named v_xs f =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (f x named))
          Left v_ys -> liftVariant $ toVarOfNamed named v_ys f
  

stWithName :: x -> Named name -> St x name
stWithName x _ = St x

toVarOfSt :: (ToVarOfNamed St xs name) => Named name -> V xs -> V (ListOfNamed St name xs)           
toVarOfSt named v_xs = toVarOfNamed named v_xs stWithName

