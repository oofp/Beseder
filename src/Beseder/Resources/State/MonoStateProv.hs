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

module Beseder.Resources.State.MonoStateProv 
  ( SetState (..)
  , ModifyState (..)
  , MonoStateProv (..) 
  , StateRes (..)
  , getState 
  ) where  

import           Protolude
import           qualified GHC.Show (Show (..))
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common

newtype SetState s = SetState s deriving (Show, Eq)
newtype ModifyState s = ModifyState (s -> s) 
instance Show (ModifyState s) where
  show _ = "Modify"

newtype StateRes p s = StateRes (p s)  deriving Show 

class MonoStateProv (p :: * -> *) s (m :: * -> *) where
  data MonoStateData (p :: * -> *) s m 
  
  createState :: StateRes p s  -> m (MonoStateData p s m)

  setState :: SetState s -> (MonoStateData p s m) -> m (MonoStateData p s m)
  modifyState :: ModifyState s -> (MonoStateData p s m) -> m (MonoStateData p s m)

  clearState :: MonoStateData p s m -> m ()

  getDataState :: MonoStateData p s m -> m s

--
type MonoState p s m name = St (MonoStateData p s m) name  

instance 
  ( Monad m
  , MonoStateProv p s m
  ) => CreateRes m name (StateRes p s) (V '[MonoState p s m name])  where
  createRes _nm p = fmap (variantFromValue . St) (createState p)  

instance 
  ( Monad m
  , MonoStateProv p s m
  ) => MkRes m (StateRes p s) where
  type ResSt m (StateRes p s) = MonoStateData p s m 
  mkRes p = createState p   

type instance StateTrans (MonoState p s m name) = 'Static

instance 
  ( Monad m
  , MonoStateProv p s m
  ) => Request m (SetState s) (MonoState p s m name) where
  type instance ReqResult (SetState s) (MonoState p s m name) = '[MonoState p s m name]
  request setStatePars (St evData) = fmap (variantFromValue . St) (setState setStatePars evData) 

instance 
  ( Monad m
  , MonoStateProv p s m
  ) => Request m (ModifyState s) (MonoState p s m name) where
  type instance ReqResult (ModifyState s) (MonoState p s m name) = '[MonoState p s m name]
  request reqPars (St evData) = fmap (variantFromValue . St) (modifyState reqPars evData) 

instance 
  ( MonadIO m
  , MonoStateProv p s m
  ) => TermState m (MonoState p s m name) where
  terminate (St evData) = Beseder.Resources.State.MonoStateProv.clearState evData

getState :: MonoStateProv p s m => MonoState p s m name -> m s
getState  (St stData) = getDataState stData   
