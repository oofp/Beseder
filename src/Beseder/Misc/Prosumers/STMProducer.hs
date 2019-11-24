{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.Misc.Prosumers.STMProducer where

import           Protolude
import           Beseder.Misc.Prosumers.Producer
import           Beseder.Misc.Prosumers.AsyncProducer
import           Beseder.Base.Common

stmProducer :: (TaskPoster m, Eq a) => STM a -> m (Producer m a)
stmProducer stm_a = 
    initAsyncProducer2 actionFunc
  where  
    actionFunc a_maybe = do 
      res <- atomically $ do
        a <- stm_a
        if (Just a == a_maybe) 
            then retry
            else return a   
      liftIO $ putStrLn ("stmProducer produced" :: Text)      
      return res
