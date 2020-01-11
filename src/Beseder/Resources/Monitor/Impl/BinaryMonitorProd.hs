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

module Beseder.Resources.Monitor.Impl.BinaryMonitorProd where

import           Protolude  
import           Haskus.Utils.Variant
import           Beseder.Misc.Prosumers.Producer
import           Beseder.Resources.Monitor.BinaryMonitorRes
--import           qualified GHC.Show (Show (..))

data BinaryMonitorProd = BinaryMonitorProd 

instance Monad m => BinaryMonitorProv m BinaryMonitorProd where
  data  BinMonitorOn m BinaryMonitorProd = BinMonitorOn (Producer m Bool)
  data  BinMonitorOff m BinaryMonitorProd = BinMonitorOff (Producer m Bool)
  data  BinMonitorStopped m BinaryMonitorProd = BinMonitorStopped
  data  ResPar m BinaryMonitorProd = MkBinaryMonitorProd (Producer m Bool) 

  createBinaryMonitor (MkBinaryMonitorProd prod) = return $ BinMonitorOff prod
  stopBinaryMonitorOn StopMonitor (BinMonitorOn prod) = do 
    produce prod Nothing
    return $ variantFromValue BinMonitorStopped
  stopBinaryMonitorOff StopMonitor (BinMonitorOff prod) = do 
    produce prod Nothing
    return $ variantFromValue BinMonitorStopped
  onOffTransition (BinMonitorOn prod) cb = 
    (produce prod) (Just (\fl ->
      when (not fl) (void $ cb (variantFromValue (BinMonitorOff prod)))))
  offOnTransition (BinMonitorOff prod) cb = 
    (produce prod) (Just (\fl ->
      when fl (void $ cb (variantFromValue (BinMonitorOn prod)))))
  termBinaryMonitor BinMonitorStopped = return ()

        
