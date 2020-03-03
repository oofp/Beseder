{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeOperators             #-}


module Beseder.Utils.TypeHelper 
  ( typeName
  , ListNames (..)
  ) where

import           Protolude hiding (TypeError)

typeName :: forall a. Typeable a => Text 
typeName = show . typeRep $ Proxy @a 

class ListNames (xs :: [*]) where
  listNames :: Proxy xs -> [Text]

instance ListNames '[] where
  listNames _ = []

instance (Typeable x, ListNames xs) => ListNames (x ': xs) where
  listNames _ = 
    let pxs :: Proxy xs
        pxs = Proxy
    in typeName @x : (listNames pxs)
