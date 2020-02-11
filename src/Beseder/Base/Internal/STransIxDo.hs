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
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.Base.Internal.STransIxDo 
  ( module Beseder.Base.Internal.STransIx
  --, return
  --, (>>)
  --, (>>=)
  , pumpEvents
  , handleEvents
  , skipAll
  , skipTo
  , clearAllResources
  , HandleEvents
  ) where

import           Protolude hiding (Product, handle,TypeError,First, (>>), (>>=), return,on)
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransIx
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.Classes
import           Beseder.Utils.ListHelper
import           Haskus.Utils.Types.List
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)
import           Beseder.Base.Internal.STransMonad ((>>))

{-
return :: Monad (q m) => a -> STrans q m sp xs xs '[] (ReturnFunc a) a
return = returnT

(>>=) :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
  --, '(rs1,ex1) ~ Eval (f1 sp xs)
  --, '(rs2,ex2) ~ Eval (f2 sp rs1)  
  ) => STrans q m sp xs rs1 ex1 f1 a -> (a -> STrans q m sp rs1 rs2 ex2 f2 b) -> STrans q m sp xs rs2 (Concat ex1 ex2) (BindFunc f1 f2) b
(>>=) = bindT

(>>) :: 
  ( Monad (q m)
  , KnownNat (Length ex1)
  --, '(rs1,ex1) ~ Eval (f1 sp xs)
  --, '(rs2,ex2) ~ Eval (f2 sp rs1)  
  ) => STrans q m sp xs rs1 ex1 f1 () -> STrans q m sp rs1 rs2 ex2 f2 b -> STrans q m sp xs rs2 (Concat ex1 ex2) (ComposeFunc f1 f2) b
(>>) = composeT
-}

pumpEvents ::
  ( _  
  ) => STrans (ContT Bool) m sp xs _ _ (EmbedFunc Dynamics (HandleLoopFunc GetNextAllFunc)) ()   
pumpEvents = Beseder.Base.Internal.STransIx.try @Dynamics (handleLoop nextEv')

type HandleEvents f_hnd =
  (EmbedFunc Dynamics
    (HandleLoopFunc
      (ComposeFunc (CaptureFunc Dynamics GetNextAllFunc) f_hnd)))

handleEvents ::
  (  _ 
  ) => STrans (ContT Bool) m (sp :&& Dynamics) _ _ _ f_hnd () -> STrans (ContT Bool) m sp xs _ _ (HandleEvents f_hnd) ()
handleEvents hnd = 
  Beseder.Base.Internal.STransIx.try @Dynamics $ 
    handleLoop (nextEv >> hnd)

skipAll :: forall n sp m xs rs ex f. 
  ( n ~ TotalSteps (sp :&& Dynamics) xs GetNextAllFunc
  , NextSteps n m (sp :&& Dynamics) xs rs ex f
  , _
  ) => STrans (ContT Bool) m sp xs _ _ _ ()
skipAll = 
  let px_n :: Proxy (TotalSteps (sp :&& Dynamics) xs GetNextAllFunc)
      px_n = Proxy
  in Beseder.Base.Internal.STransIx.try @Dynamics (nextSteps px_n)

skipTo' :: forall sp1 n sp m xs rs ex f. 
  ( n ~ TotalSteps (sp :&& (Not sp1)) xs GetNextAllFunc
  , NextSteps n m (sp :&& (Not sp1)) xs rs ex f
  , _
  ) => sp1 -> STrans (ContT Bool) m sp xs _ _ _ ()
skipTo' _sp1 = 
  let px_n :: Proxy (TotalSteps (sp :&& (Not sp1)) xs GetNextAllFunc)
      px_n = Proxy
  in Beseder.Base.Internal.STransIx.try @(Not sp1) (nextSteps px_n)

skipTo :: forall sp1 sp m xs. 
  ( GetInstance sp1
  , _
  ) => STrans (ContT Bool) m sp xs _ _ _ ()
skipTo  = 
  let sp1 :: sp1
      sp1 = getInstance
  in skipTo' sp1

clearAllResources :: 
  ( sp1 ~ Statics
  , ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , TermState m (ClrVar xs_sub)
  , MonadTrans q
  , _
  ) => STrans q m sp xs (Union '[()] ex_sub) ('[]) (CaptureFunc Statics ClearAllVarFunc) ()
clearAllResources = 
    Beseder.Base.Internal.STransIx.on @(Statics) termAndClearAllResources 
  
