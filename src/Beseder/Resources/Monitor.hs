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

module Beseder.Resources.Monitor 
  ( module Beseder.Resources.Monitor.BinaryMonitorRes
  , module Beseder.Resources.Monitor.Impl.BinaryMonitorProd
  ) where

import           Beseder.Resources.Monitor.BinaryMonitorRes
import           Beseder.Resources.Monitor.Impl.BinaryMonitorProd