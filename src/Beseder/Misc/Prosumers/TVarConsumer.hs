{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.Misc.Prosumers.TVarConsumer where

import           Protolude
import           Control.Concurrent.STM.TVar
import           Beseder.Misc.Prosumers.Consumer

newtype TVarConsumer a = TVarConsumer (TVar a)

instance (MonadIO m) => GetConsumer (TVarConsumer a) m a where
  consumer (TVarConsumer ta) =
    let 
      consumeFunc Nothing =    return ()
      consumeFunc (Just a) =   liftIO $ atomically $ writeTVar ta a
    in return $ Consumer consumeFunc

