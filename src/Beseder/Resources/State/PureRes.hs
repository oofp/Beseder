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

module Beseder.Resources.State.PureRes 
  ( MkPureRes (..)
  , Op (..)
  , OpOne (..)
  , PureRes (..)
  ) where

import           Protolude  
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import           qualified GHC.Show (Show (..))
 
newtype PureRes p = PureRes p deriving Show

class MkPureRes pureRes  where
  type PureResInitState pureRes :: *
  mkPureRes :: pureRes -> PureResInitState pureRes  

class Op op st where
  type OpResults (op :: *) (st :: *) :: [*]
  opReq :: op -> st -> V (OpResults op st)

class OpOne op st where
  type OpResult (op :: *) (st :: *) :: *
  opReqOne :: op -> st -> OpResult op st
  
----------------------------------------------------------------------------------------------------------  
instance (Monad m, MkPureRes pureRes) => MkRes m (PureRes pureRes)  where
  type ResSt m (PureRes pureRes)  = PSt (PureResInitState pureRes)  
  mkRes (PureRes pureRes ) = pure $ PSt $ mkPureRes pureRes

type family PureStList name (dataList :: [*]) where
  PureStList name '[] = '[]
  PureStList name (stData ': dataLst) = (St (PSt stData) name) ': (PureStList name dataLst)
  
class PureStVar xs name where
  asPureStVar :: Named name -> V xs -> V (PureStList name xs) 
  
instance PureStVar ('[]) name where
  asPureStVar named _ = undefined

newtype PSt a = PSt a

pureStWithName :: stData -> Named name -> St (PSt stData) name
pureStWithName stData named = St (PSt stData)
  
instance 
  ( PureStVar xs name 
  , Liftable '[St (PSt x) name] (PureStList name (x ': xs))
  , Liftable (PureStList name xs) (PureStList name (x ': xs))) => PureStVar (x ': xs) name where
    asPureStVar named v_xs =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (pureStWithName x named))
          Left v_ys -> liftVariant $ asPureStVar named v_ys 
  

instance 
  ( Op op st
  , Monad m, PureStVar (OpResults op st) name
  ) => Request m op (St (PSt st) name) where
  type ReqResult op  (St (PSt st) name) = PureStList name (OpResults op st)
  request op st@(St (PSt stData)) = return $ (asPureStVar (nameFromSt st)) (opReq op stData)

  
instance 
  ( OpOne op st
  ) => Op op st where
  type OpResults op st = '[OpResult op st]
  opReq op st = variantFromValue (opReqOne op st) 


type instance StateTrans (St (PSt st) name) = 'Static
instance (Monad m) => TermState m (St (PSt st) name) where
  terminate _ = return ()