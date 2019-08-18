{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.STransInstr where

import           Protolude                    hiding (Product, handle,TypeError,First)
import           Control.Monad.Cont
import           Control.Monad.Identity
import           Haskus.Utils.Flow
import           Data.Text
import           Data.Typeable
import           GHC.TypeLits
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Type.Errors hiding (Eval,Exp)

import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.STrans
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Utils.ListHelper
import           Beseder.Utils.VariantHelper
import           Beseder.Base.Internal.SplitFlow
import           Beseder.Utils.Lst

instrumentTrans ::
  ( MonadTrans q
  , Monad m
  , Eval (c xs m)
  , InstrumentorCs c sp xs m f
  ) => Instrumentor m c -> STrans q m sp xs rs_ex f a -> STrans q m sp xs rs_ex f a 
instrumentTrans f (ComposeTrans t1 t2) = ComposeTrans (instrumentTrans f t1) (instrumentTrans f t2)
instrumentTrans f (BindTrans t1 fnc) = BindTrans (instrumentTrans f t1) (InstrumentTrans f . fnc)
instrumentTrans f (EmbedTrans sp t) = EmbedTrans sp (instrumentTrans f t)
instrumentTrans f (CaptureTrans sp t) = CaptureTrans sp (instrumentTrans f t)
instrumentTrans f (IffTrans cond t) = IffTrans cond (instrumentTrans f t)
instrumentTrans f (IfElseTrans cond t1 t2) = IfElseTrans cond (instrumentTrans f t1) (instrumentTrans f t2)
instrumentTrans instr (ForeverTrans t) = ForeverTrans (instrumentTrans instr t)
instrumentTrans instr t = InstrumentTrans instr t


