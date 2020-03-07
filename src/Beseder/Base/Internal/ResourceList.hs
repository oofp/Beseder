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

module Beseder.Base.Internal.ResourceList where

import           GHC.TypeLits
import           Beseder.Base.Internal.Core
import           Protolude hiding (TypeError)
import           Beseder.Base.Internal.Named
import           Haskus.Utils.Variant
import qualified Prelude as SafeUndef (undefined) 

--- Get

class GetResource resList name where
  type GetResourceRes resList name
  getResource :: resList -> Named name -> GetResourceRes resList name

instance GetResource (St st name) name where
  type instance GetResourceRes (St st name) name = (St st name)
  getResource st _name = st

instance GetResource' (St st name1, more) name ((IsNameMatch name1 name)) => GetResource (St st name1, more) name where
  type instance GetResourceRes (St st name1, more) name = GetResourceRes' (St st name1, more) name (IsNameMatch name1 name)
  getResource resList named = getResource' resList named (Proxy @(IsNameMatch name1 name))


class GetResource' st name (fl :: Bool) where
  type GetResourceRes' st name fl
  getResource' :: st -> Named name -> Proxy fl -> GetResourceRes' st name fl

instance GetResource' (St st name, more) name 'True where
  type instance GetResourceRes' (St st name, more) name 'True = (St st name)
  getResource' (st, _more) _name _ = st

instance GetResource more name => GetResource' (St st name1, more) name 'False where
  type instance GetResourceRes' (St st name1, more) name 'False = GetResourceRes more name
  getResource' (_st1, more) name _ = getResource more name

--- Append
class AppendResource resList newRes where
  type AppendResourceRes resList newRes 
  appendResource :: resList -> newRes -> AppendResourceRes resList newRes

instance AppendResource () (St res name) where
  type AppendResourceRes () (St res name) = (St res name) 
  appendResource () newRes = newRes

instance AppendResource lst () where
  type AppendResourceRes lst () = lst 
  appendResource lst () = lst

type family IsNameDifferent (n1 :: Symbol) (n2 :: Symbol) :: Constraint where
  IsNameDifferent n1 n1 = TypeError ('Text "Resource " :<>: 'Text n1 :<>: 'Text " already exists") 
  IsNameDifferent n1 n2 = ()

type family IsNameMatch (n1 :: Symbol) (n2 :: Symbol) :: Bool where
  IsNameDifferent n1 n1 = True
  IsNameDifferent n1 n2 = False
  
instance IsNameDifferent name1 name2 => AppendResource (St r1 name1) (St r2 name2) where
  type AppendResourceRes (St r1 name1) (St r2 name2) = (St r1 name1, St r2 name2)
  appendResource r1 r2 = (r1,r2)

instance 
  ( IsNameDifferent name1 name2
  , AppendResource  tail (St r2 name2)
  ) => AppendResource (St r1 name1, tail) (St r2 name2) where
  type AppendResourceRes (St r1 name1, tail) (St r2 name2) = (St r1 name1, AppendResourceRes tail (St r2 name2))
  appendResource (r1, tail) r2 = (r1, appendResource tail r2)

instance 
  ( AppendResource  lst (St r1 name1)
  , AppendResource  (AppendResourceRes lst (St r1 name1)) tail
  ) => AppendResource lst (St r1 name1, tail) where
  type AppendResourceRes lst (St r1 name1, tail) = AppendResourceRes (AppendResourceRes lst (St r1 name1)) tail
  appendResource lst (r1, tail) = appendResource (appendResource lst r1) tail
  
-- Remove resource
class RemoveResource resList (resName :: Symbol) where
  type RemoveResourceRes resList resName 
  removeResource :: resList -> Named resName -> RemoveResourceRes resList resName
  
instance RemoveResource (St res name) name where
  type RemoveResourceRes (St res name) name = () 
  removeResource _res _named = ()

class RemoveResource' resList (resName :: Symbol) (fl :: Bool) where
  type RemoveResourceRes' resList resName fl
  removeResource' :: resList -> Named resName -> Proxy fl -> RemoveResourceRes' resList resName fl

instance 
  ( fl ~ IsNameMatch resName resName1
  , RemoveResource' (St res resName, tail) (resName1 :: Symbol) fl
  ) => RemoveResource (St res resName, tail) resName1 where
  type RemoveResourceRes (St res resName, tail) resName1 = RemoveResourceRes' (St res resName, tail) resName1 (IsNameMatch resName resName1)
  removeResource lst named = removeResource' lst named (Proxy @(IsNameMatch resName resName1)) 
  
    
instance RemoveResource' (St res resName, tail) resName1 'True where
  type RemoveResourceRes' (St res resName, tail) resName1 'True = tail 
  removeResource' (_res, tail) _named _px = tail

instance 
  ( RemoveResource tail resName1
  , AppendResource (St res resName) (RemoveResourceRes tail resName1) 
  ) => RemoveResource' (St res resName, tail) resName1 'False where
  type RemoveResourceRes' (St res resName, tail) resName1 'False = AppendResourceRes (St res resName) (RemoveResourceRes tail resName1) 
  removeResource' (res, tail) named _px = appendResource res (removeResource tail named)


--
-- Rename resource
type family IsNameUnique resList (n :: Symbol) :: Constraint where
  IsNameUnique () n = ()
  IsNameUnique (St res n) n = TypeError ('Text "Resource " :<>: 'Text n :<>: 'Text " already exists") 
  IsNameUnique (St res n, tail) n = TypeError ('Text "Resource " :<>: 'Text n :<>: 'Text " already exists") 
  IsNameUnique (St res n1) n = ()
  IsNameUnique (St res n1, tail) n = IsNameUnique tail n

class RenameResource resList (resName :: Symbol) (newName :: Symbol) where
  type RenameResourceRes resList resName newName
  renameResource :: resList -> Named resName -> Named newName -> RenameResourceRes resList resName newName

class RenameResource' resList (resName :: Symbol) (newName :: Symbol) (fl :: Bool) where
  type RenameResourceRes' resList resName newName fl
  renameResource' :: resList -> Named resName -> Named newName -> Proxy fl -> RenameResourceRes' resList resName newName fl

instance RenameResource (St res resName) resName newName where
  type RenameResourceRes (St res resName) resName newName = (St res newName) 
  renameResource (St res) _ _ = (St res)

{-
instance (fl ~ IsNameMatch resName1 resName, RenameResource' (St res resName1, tail) resName newName fl, IsNameUnique tail newName) => RenameResource (St res resName1, tail) resName newName where
  type RenameResourceRes (St res resName1,tail) resName newName = RenameResourceRes' (St res resName,tail) resName newName (IsNameMatch resName1 resName)  
  renameResource resList resName newName = renameResource resList resName newName (Proxy @(IsNameMatch resName1 resName))
-}
instance 
  ( fl ~ IsNameMatch resName resName1
  , RenameResource' (St res resName, tail) (resName1 :: Symbol) newName fl
  , IsNameUnique (St res resName, tail) newName
  ) => RenameResource (St res resName, tail) resName1 newName where
  type RenameResourceRes (St res resName, tail) resName1 newName = RenameResourceRes' (St res resName, tail) resName1 newName (IsNameMatch resName resName1)
  renameResource lst named newName = renameResource' lst named newName (Proxy @(IsNameMatch resName resName1)) 
  

instance RenameResource' (St res resName1, tail) resName newName 'True where
  type RenameResourceRes' (St res resName1,tail) resName newName 'True = (St res newName, tail) 
  renameResource' (St res, tail) _ _ _ = (St res, tail)

instance RenameResource tail resName newName => RenameResource' (St res resName1, tail) resName newName 'False where
  type RenameResourceRes' (St res resName1,tail) resName newName 'False = (St res resName1, RenameResourceRes tail resName newName) 
  renameResource' (St res, tail) resName newName _ = (St res, renameResource tail resName newName)

instance RenameResource (V '[]) resName newName where
  type RenameResourceRes (V '[]) resName newName = (V '[])
  renameResource _ _ _ = SafeUndef.undefined

instance 
    ( RenameResource x resName newName
    , RenameResource (V xs) resName newName
    , V (RenameResourceList xs resName newName) ~ RenameResourceRes (Variant xs) resName newName
    , Liftable '[RenameResourceRes x resName newName] (RenameResourceList (x ': xs) resName newName)
    , Liftable (RenameResourceList xs resName newName) (RenameResourceList (x ': xs) resName newName)
    ) => RenameResource (V (x ': xs)) resName newName where
  type RenameResourceRes (V (x ': xs)) resName newName = V (RenameResourceList (x ': xs) resName newName) 
  renameResource v_x_xs resName newName = 
      case popVariantHead v_x_xs of 
        Right x -> liftVariant $ variantFromValue $ renameResource x resName newName
        Left v_xs -> 
          let newList :: V (RenameResourceList xs resName newName)
              newList = renameResource v_xs resName newName 
          in liftVariant newList 

type family RenameResourceList (xs :: [*]) resName newName where
  RenameResourceList '[] resName newName = '[]
  RenameResourceList (x ': xs) resName newName = (RenameResourceRes x resName newName) ': (RenameResourceList xs resName newName)          
