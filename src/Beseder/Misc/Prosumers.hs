{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Beseder.Misc.Prosumers
  ( Consumer (..)
  , Producer (..) 
  , STMConsumer (..)
  , stmProducer
  ) where

import Beseder.Misc.Prosumers.Producer    
import Beseder.Misc.Prosumers.Consumer    
import Beseder.Misc.Prosumers.TVarConsumer    
import Beseder.Misc.Prosumers.STMConsumer    
import Beseder.Misc.Prosumers.STMProducer    
import Beseder.Misc.Prosumers.AsyncProducer    
