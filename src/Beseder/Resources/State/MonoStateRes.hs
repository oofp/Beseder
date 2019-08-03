module Beseder.Resources.State.MonoStateRes 
  ( module Beseder.Resources.State.MonoStateProv 
  , module Beseder.Resources.State.Impl.IORefStateProv    
  , module Beseder.Resources.State.Impl.STMStateProv    
  , module Beseder.Resources.State.Impl.PureStateProv   
  ) where

import Beseder.Resources.State.MonoStateProv 
      ( SetState (..)
      , ModifyState (..)
      , MonoStateProv
      , StateRes 
      , getState 
      ) 
import Beseder.Resources.State.Impl.IORefStateProv    
import Beseder.Resources.State.Impl.STMStateProv    
import Beseder.Resources.State.Impl.PureStateProv    