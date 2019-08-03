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

module Beseder.Base.Internal.Named where

import           Data.Text
import           GHC.Exts
import           GHC.TypeLits
import           Protolude
import           qualified GHC.Show (Show (..))

data Named (name :: Symbol) = Named

instance l ~ l' =>
         IsLabel (l :: Symbol) (Named l') where
  fromLabel = Named

namedFromProxy :: Proxy name -> Named name
namedFromProxy _px = Named

instance (KnownSymbol name) => Show (Named name) where
  show _nm = 
    let px = Proxy @name
    in symbolVal px

getName :: (KnownSymbol name) => Named name -> Text
getName nm = pack (show nm)

type family GetName named :: Symbol where
  GetName (Named name) = name


