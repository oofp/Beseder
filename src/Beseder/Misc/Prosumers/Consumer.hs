{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Beseder.Misc.Prosumers.Consumer where

import           Protolude
import           Data.Functor.Contravariant

newtype Consumer m a = 
  Consumer 
    { consume :: Maybe a -> m ()
    } 

instance Contravariant (Consumer m) where
  contramap f_b_a c_a = Consumer (\mb -> (consume c_a) (fmap f_b_a mb))    

class GetConsumer gc m a where
  consumer :: gc -> m (Consumer m a)      