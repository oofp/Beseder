{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor          #-}

module Beseder.Misc.Prosumers.Producer where

import           Protolude

newtype Producer m a = 
  Producer 
    { produce :: Maybe (a -> m ()) -> m ()
    } deriving Functor 

    
class GetProducer gp m a where
  producer :: gp -> m (Producer m a)    