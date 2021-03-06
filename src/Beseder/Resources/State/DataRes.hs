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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveFunctor         #-}

module Beseder.Resources.State.DataRes
  ( InitData (..)
  , SetData (..)
  , ModifyData (..) 
  , StD
  , setDataProxy
  , getData
  , setTrue 
  , setFalse
  , SetTrue
  , SetFalse
  , IsTrue 
  , IsFalse 
  , initAsTrue 
  , initAsFalse
  , InitAsTrue
  , InitAsFalse
  , NextTags
  , SetTag
  , GetTagType
  , modifyTag
  , getTag
  , newTagRes
  , IsTag
  ) where  

import           GHC.TypeLits
import           Protolude hiding (TypeError)
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Control.Arrow (Kleisli (..))
import qualified GHC.Show (Show (..))
import           Data.Coerce (coerce)
import           Beseder.Base.ControlData (newRes, STransData, NewResFunc)

newtype D a = D a deriving (Show, Eq)

newtype InitData a = InitData a deriving (Show, Eq)
instance GetInstance a => GetInstance (InitData a) where getInstance = InitData getInstance 

instance (Monad m) => CreateRes m name (InitData a) (V '[(St (D a) name)]) where
  createRes _named  (InitData a) = return (variantFromValue $ St (D a))

instance 
  ( Monad m
  ) => MkRes m (InitData a) where
  type ResSt m (InitData a) = D a  
  mkRes (InitData a) = return $ D a   

newtype SetData a = SetData a deriving (Show, Eq)
data ModifyData a b = ModifyData (a -> b) 
  
instance Show (ModifyData a b) where
  show _ = "Modify"

instance GetInstance a => GetInstance (SetData a) where
  getInstance = SetData getInstance
  
instance (Monad m) => Request m (SetData a) (St (D b) name) where
  type ReqResult (SetData a) (St (D b) name) = '[St (D a) name]
  request (SetData a) (St _) = return $ variantFromValue (St (D a))
instance (Monad m) => Request m (ModifyData a b) (St (D a) name) where
  type ReqResult (ModifyData a b) (St (D a) name) = '[St (D b) name]
  request (ModifyData f) (St (D a)) = return $ variantFromValue (St (D (f a)))
  

--type family SupportedRequestsFam (st :: *) :: [*]

--type SetDataAny = forall b. SetData b  
--type ModifyDataAny a = forall c. ModifyData a c
type instance Eval (SupportedRequests (St (D (Proxy (fl :: Bool))) name)) = [SetTrue, SetFalse] --'[SetData Any, ModifyData a Any]
--type instance SupportedRequestsFam (St (D a) name) = forall b. '[SetData b, ModifyData a b]

type instance StateTitle (D (Proxy 'True)) = "true"
type instance StateTitle (D (Proxy 'False)) = "false"
--type instance StateTitle (D a) = "Data"
  
getData :: St (D a) name -> a
getData (St (D a)) = a  
  
type StD a name = St (D a) name 
type instance StateTrans (St (D a) name) = 'Static
instance Monad m => TermState m (St (D a) name) where
  terminate _ = return ()

--
instance (Monad m) => Request m (Kleisli m a b) (St (D a) name) where
  type ReqResult (Kleisli m a b) (St (D a) name) = '[St (D b) name]
  request (Kleisli f) (St (D a)) = do 
    b <- f a
    return $ variantFromValue (St (D b))

--
type InitDataPx (p :: k) = InitData (Proxy p)
type SetDataPx p = SetData (Proxy p)

initDataProxy :: forall p. InitDataPx p
initDataProxy = InitData Proxy

setDataProxy :: forall p. SetDataPx p
setDataProxy = SetData Proxy

type family IsDataFam p a :: Bool where
  IsDataFam p (St (D (Proxy p)) _) = 'True
  IsDataFam p _ = 'False
  
data IsData :: p -> Type -> Exp Bool 
type instance Eval (IsData p a) = IsDataFam p a

type InitAsTrue = InitDataPx 'True 
type InitAsFalse = InitDataPx 'False 
type SetTrue =  SetDataPx 'True
type SetFalse = SetDataPx 'False
type IsTrue = IsData 'True
type IsFalse = IsData 'False

initAsTrue :: InitAsTrue
initAsTrue = initDataProxy @True

initAsFalse :: InitAsFalse
initAsFalse = initDataProxy @False

setTrue :: SetTrue
setTrue = SetData (Proxy @True)

setFalse :: SetFalse
setFalse = SetData (Proxy @False)

instance GetInstance (InitDataPx a) where
  getInstance = InitData Proxy

instance GetInstance SetFalse where
  getInstance = setFalse
 
instance GetInstance SetTrue where
  getInstance = setTrue
      
--
--type SetData' = forall a. SetData a

newtype Tagged a = Tagged a deriving Functor
type family NextTags a :: [*]

type instance Eval (SupportedRequests (St (D (Tagged a)) name)) = SupportedTagReqFam a (NextTags a)

type family SupportedTagReqFam (a :: *) (xs :: [*]) where
  SupportedTagReqFam a '[] = '[]
  SupportedTagReqFam a (x ': xs) = (SetData x) ':  (ModifyData a x) ': SupportedTagReqFam a xs

type SetTag a = SetData (Tagged a)
modifyTag :: (a -> b) -> ModifyData (Tagged a) (Tagged b)
modifyTag f_a_b  = ModifyData (fmap f_a_b)

type instance PropType (Tagged a) = a
instance Property (St (D (Tagged a)) name) (Tagged a) where
  getProp st _px = coerce st

getTag :: forall a name. (St (D (Tagged a)) name) -> a
getTag st = 
  let pxa :: Proxy (Tagged a)
      pxa = Proxy
  in getProp st pxa


data IsTag :: Type -> Type -> Exp Bool 
type instance Eval (IsTag t st) = IsTagFam t st

type family IsTagFam t st where 
  IsTagFam t (St (D (Tagged t)) name) = 'True
  IsTagFam t _ = 'False

type InitTag a = InitData (Tagged a)
initTag :: a -> InitTag a
initTag = coerce

type family GetTagType (st :: *) :: * where
  GetTagType (St (D (Tagged t)) name) = t
  GetTagType st = TypeError 
                    ( 'Text "Resource is not data tag `"
                      ':<>: 'ShowType st) 


newTagRes :: forall m sp a name. Named name -> a -> STransData m sp (NewResFunc (InitTag a) name m) ()
newTagRes name a = newRes name (initTag a)
