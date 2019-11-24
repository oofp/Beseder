{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.Misc.Prosumers.STMConsumer where

import           Protolude
import           Beseder.Misc.Prosumers.Consumer

newtype STMConsumer a = STMConsumer (a -> STM ())

instance (MonadIO m) => GetConsumer (STMConsumer a) m a where
  consumer (STMConsumer f) =
    let 
      consumeFunc Nothing =    return ()
      consumeFunc (Just a) =   liftIO $ atomically $ f a
    in return $ Consumer consumeFunc

