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
{-# LANGUAGE ScopedTypeVariables   #-}

module Beseder.Resources.State.ImpRes 
  ( MkImpRes (..)
  , ImpOp (..)
  , ImpRes (..)
  , ImpSt (..)
  , ImpureSt
  ) where

import           Protolude  
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Internal.Core
import           Beseder.Base.Common
import qualified Prelude as SafeUndef (undefined) 

newtype ImpRes p = ImpRes p deriving Show

class MkImpRes m impRes where
  type ImpResInitState m impRes :: *
  mkImpRes :: impRes -> m (ImpResInitState m impRes)  

class ImpOp m op st where
  type OpImpResults (op :: *) (st :: *) :: [*]
  opImpReq :: op -> st -> m (V (OpImpResults op st))

----------------------------------------------------------------------------------------------------------  
instance (Monad m, MkImpRes m impRes) => MkRes (m :: * -> *) (ImpRes impRes)  where
  type ResSt m (ImpRes impRes)  = ImpSt (ImpResInitState m impRes)  
  mkRes (ImpRes impRes) = ImpSt <$> mkImpRes impRes

type family ImpureStList name (dataList :: [*]) where
  ImpureStList name '[] = '[]
  ImpureStList name (stData ': dataLst) = (St (ImpSt stData) name) ': (ImpureStList name dataLst)
  
class ImpureStVar xs name where
  asImpureStVar :: Named name -> V xs -> V (ImpureStList name xs) 
  
instance ImpureStVar ('[]) name where
  asImpureStVar _named _ = SafeUndef.undefined

newtype ImpSt a = ImpSt a
type ImpureSt a name = St (ImpSt a) name
type instance UnwrapContent (ImpureSt a name) = a

impureStWithName :: stData -> Named name -> St (ImpSt stData) name
impureStWithName stData _named = St (ImpSt stData)
  
instance 
  ( ImpureStVar xs name 
  , Liftable '[St (ImpSt x) name] (ImpureStList name (x ': xs))
  , Liftable (ImpureStList name xs) (ImpureStList name (x ': xs))) => ImpureStVar (x ': xs) name where
    asImpureStVar named v_xs =  
      case popVariantHead v_xs of
          Right x -> liftVariant $ (variantFromValue (impureStWithName x named))
          Left v_ys -> liftVariant $ asImpureStVar named v_ys 
  

instance 
  ( ImpOp m op st
  , Monad m, ImpureStVar (OpImpResults op st) name
  ) => Request m op (St (ImpSt st) name) where
  type ReqResult op  (St (ImpSt st) name) = ImpureStList name (OpImpResults op st)
  request op st@(St (ImpSt stData)) = fmap (asImpureStVar (nameFromSt st)) (opImpReq op stData)

type instance StateTrans (St (ImpSt st) name) = 'Static
instance (Monad m) => TermState m (St (ImpSt st) name) where
  terminate _ = return ()

