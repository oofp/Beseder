{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE RankNTypes            #-}

module Beseder.Resources.Monitor.BinaryMonitorRes where

import           Protolude  
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Utils.BoolHelper
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           Beseder.Misc.Prosumers.Producer
import           qualified GHC.Show (Show (..))

instance Show (BinaryMonitor m) where
    show _ = "BinMonitor"

newtype BinaryMonitor m = BinaryMonitor (Producer m Bool) 
data BinaryMonitorRes (binState :: Bool) m = BinaryMonitorRes (Producer m Bool)

instance (MonadIO m) => MkRes m (BinaryMonitor m)  where
  type ResSt m (BinaryMonitor m)  = BinaryMonitorRes 'False m 
  mkRes (BinaryMonitor prod) = return $ BinaryMonitorRes prod 

type BinMonitorOn m name = St (BinaryMonitorRes 'True m) name
type BinMonitorOff m name = St (BinaryMonitorRes 'False m) name

data IsBinMonitorOn :: Type -> Exp Bool 
type instance Eval (IsBinMonitorOn a) = IsBinMonitorOnFam a
data IsBinMonitorOff :: Type -> Exp Bool 
type instance Eval (IsBinMonitorOff a) = IsBinMonitorOffFam a

type family IsBinMonitorOnFam a :: Bool where
  IsBinMonitorOnFam (BinMonitorOn _ _) = 'True
  IsBinMonitorOnFam _ = 'False

type family IsBinMonitorOffFam a :: Bool where
  IsBinMonitorOffFam (BinMonitorOff _ _) = 'True
  IsBinMonitorOffFam _ = 'False
  
type instance StateTrans (St (BinaryMonitorRes fl m) name) = 'Dynamic
type instance StateTrans (St BinaryMonitorStopped name) = 'Static

proxyFromRes :: St (BinaryMonitorRes fl m) name -> Proxy fl
proxyFromRes _ = Proxy

isChanged :: KnownBool fl => St (BinaryMonitorRes fl m) name -> Bool -> Bool
isChanged st curBool = 
  (boolVal (proxyFromRes st)) /= curBool

instance (MonadIO m, KnownBool flag) => Transition m (St (BinaryMonitorRes flag m) name) where
  type NextStates (St (BinaryMonitorRes flag m) name) = '[(St (BinaryMonitorRes (NotBool flag) m) name)]
  next st@(St (BinaryMonitorRes prod)) cb = do
    (produce prod) (Just (\fl ->
      when (isChanged st fl) (void $ cb (variantFromValue (St (BinaryMonitorRes prod))))))
    return True  

data StopMonitor = StopMonitor deriving Show
data BinaryMonitorStopped = BinaryMonitorStopped

instance (MonadIO m) => Request m StopMonitor (St (BinaryMonitorRes flag m) name) where
  type ReqResult StopMonitor (St (BinaryMonitorRes flag m) name) = '[St BinaryMonitorStopped name]
  request StopMonitor (St (BinaryMonitorRes prod)) = do
    produce prod Nothing
    return $ variantFromValue (St BinaryMonitorStopped)

instance Monad m => TermState m (St BinaryMonitorStopped name) where
  terminate _ = return ()

type instance TermRequest (St (BinaryMonitorRes flag m) name) = StopMonitor

