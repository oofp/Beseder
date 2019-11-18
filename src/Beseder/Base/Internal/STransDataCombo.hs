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
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE RebindableSyntax      #-}

module Beseder.Base.Internal.STransDataCombo where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                                      (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.NatOne
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransFunc
import           Beseder.Base.Internal.STransData
import           Beseder.Base.Internal.STransDataDo
import           Beseder.Base.Internal.STransFunc
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Utils.ListHelper

type ClearResIfExists name = CaptureFunc (By name) (ClearAllFunc name)

type family ClearResourcesFam (names :: [Symbol]) :: (* -> [*] -> Exp ([*],[*])) where
    ClearResourcesFam '[] = NoopFunc
    ClearResourcesFam '[name] = ClearResIfExists name -- CaptureFunc (By name) (ClearAllFunc name)
    ClearResourcesFam (name ': tail) = ComposeFunc (ClearResIfExists name) (ClearResourcesFam tail)

data ClearScopedComp :: [*] -> [*] -> Exp (* -> [*] -> Exp ([*],[*]))

type instance Eval (ClearScopedComp xs ys) =
    ClearResourcesFam (Subtract (GetAllNames ys) (GetAllNames xs)) 

clearResources :: 
    ( clearFunc ~ ClearResourcesFam names
    , ReifyTrans m sp clearFunc () 
    ) => Proxy (names :: [Symbol]) -> STransData m sp clearFunc ()
clearResources _ = 
    let pxNames :: Proxy clearFunc 
        pxNames = Proxy
    in reifyTrans pxNames

scopeRes :: STransData m sp f () -> STransData m sp (ScopeFunc f ClearScopedComp) () 
scopeRes st = Scope st (Proxy @ClearScopedComp)

data ClearAllButComp :: [Symbol] -> [*] -> Exp (* -> [*] -> Exp ([*],[*]))
type instance Eval (ClearAllButComp names xs) =
    ClearResourcesFam (Subtract (GetAllNames xs) names) 

clearAllBut :: Proxy (names :: [Symbol]) -> STransData m sp (FuncFunc (ClearAllButComp names)) () 
clearAllBut px = Func Proxy   