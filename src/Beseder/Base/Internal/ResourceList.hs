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
import           GHC.Exts
import           Beseder.Base.Internal.Named
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.TypeExp
import           Data.Function (id)
import           Prelude (error)

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
  removeResource res _named = ()

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
  removeResource' (res, tail) _named _px = tail

instance 
  ( RemoveResource tail resName1
  , AppendResource (St res resName) (RemoveResourceRes tail resName1) 
  ) => RemoveResource' (St res resName, tail) resName1 'False where
  type RemoveResourceRes' (St res resName, tail) resName1 'False = AppendResourceRes (St res resName) (RemoveResourceRes tail resName1) 
  removeResource' (res, tail) named _px = appendResource res (removeResource tail named)


