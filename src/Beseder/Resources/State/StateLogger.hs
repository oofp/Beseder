{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Beseder.Resources.State.StateLogger 
  ( LogState (..)
  , LogEntries
  , StateLoggerRes (..)
  ) where  

import           Protolude
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common

--

data LogEntriesList (logEntries :: [(Symbol,[*])]) (m :: * -> *) = LogEntriesList 
type LogEntries logEntries m name = St (LogEntriesList logEntries m) name  


data StateLoggerRes = StateLoggerRes deriving Show
instance GetInstance StateLoggerRes where
    getInstance = StateLoggerRes
    
instance 
  ( Monad m
  ) => MkRes m (StateLoggerRes) where
  type ResSt m StateLoggerRes = LogEntriesList '[] m 
  mkRes _ = return LogEntriesList  

type instance StateTrans (LogEntries logEntries m name) = 'Static

data LogState (label :: Symbol) (states ::[*]) = LogState deriving Show

type family AppendLogEntry (entry :: (Symbol,[*])) (logEntries :: [(Symbol,[*])]) :: [(Symbol,[*])] where
  AppendLogEntry '(label, sts) '[] = '[('(label, sts))]
  AppendLogEntry '(label, sts)  ('(label, sts) ': logEntries) = ('(label, sts) ': logEntries)
  AppendLogEntry '(label, sts)  ('(label1, sts1) ': logEntries) = ('(label1, sts1) ': (AppendLogEntry '(label, sts) logEntries))

instance 
  ( Monad m
  ) => Request m (LogState label states) (LogEntries logEntries m name)  where
  type instance ReqResult (LogState label states) (LogEntries logEntries m name) = '[LogEntries (AppendLogEntry '(label,states) logEntries) m name] -- '[LogEntries ('(label,states) ': logEntries) m name]
  request LogState (St _) = return $ variantFromValue (St LogEntriesList) 

instance 
  ( MonadIO m
  ) => TermState m (LogEntries logEntries m name) where
  terminate (_) = return ()

