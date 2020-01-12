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

module Beseder.Resources.Switch 
  ( module Beseder.Resources.State.BinarySwitchRes
  , module Beseder.Resources.State.Impl.BinarySwitchCons 
  )
  where

import           Beseder.Resources.State.BinarySwitchRes
import           Beseder.Resources.State.Impl.BinarySwitchCons 
