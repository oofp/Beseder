{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.Misc.Prosumers.IntervalProducer where

import           Protolude
import           Beseder.Misc.Prosumers.Producer
import           Beseder.Misc.Prosumers.AsyncProducer
import           Beseder.Base.Common

intervalProducer :: (TaskPoster m, Eq a) => Int -> a -> (a -> a) -> m (Producer m a)
intervalProducer timeoutSec initVal func = 
    initAsyncProducer2 actionFunc
  where  
    actionFunc a_maybe = do 
      threadDelay (timeoutSec*1000000)
      return $ 
        case a_maybe of 
          Nothing -> initVal
          Just a -> func a 
