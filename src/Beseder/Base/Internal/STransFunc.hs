{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.STransFunc where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Haskus.Utils.Types.List
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransData
import           Beseder.Base.Internal.NatOne

class ReifyTrans m sp (sfunc :: * -> [*] -> Exp ([*],[*])) a where
  reifyTrans :: Proxy sfunc -> STransData m sp sfunc a 

instance ReifyTrans m sp (ClearAllFunc name) () where
  reifyTrans _ = 
      let named :: Named name 
          named = Named 
      in Clear named

instance (ReifyTrans m sp f1 (), ReifyTrans m sp f2 ()) => ReifyTrans m sp (ComposeFunc f1 f2) () where
  reifyTrans _ =  Compose (reifyTrans (Proxy @f1)) (reifyTrans (Proxy @f2))


instance (ReifyTrans m sp f_sub ()) => ReifyTrans m sp (CaptureFunc sp1 f_sub) () where
  reifyTrans _ =  On @sp1 (reifyTrans (Proxy @f_sub))
  