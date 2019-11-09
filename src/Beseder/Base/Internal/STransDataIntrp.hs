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
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.Base.Internal.STransDataIntrp 
  ( interpret
  , Interpretable 
  ) where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on, liftIO)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Trans (MonadTrans)
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.STransIx
import           Beseder.Base.Internal.Flow hiding (newRes)
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STransDef
import           Beseder.Base.Internal.STransData 
import           Beseder.Utils.VariantHelper

type NewResCon name resPars q m sp xs rs ex = NewResConFam name resPars (St (ResSt m resPars) name) m sp xs rs ex
type family NewResConFam name resPars res m sp xs rs ex where
  NewResConFam name resPars res m sp xs rs ex = NewResConFam' name resPars res m sp xs rs ex (AppendToTupleList xs res) 
type family NewResConFam' name resPars res m sp xs rs ex zs where
  NewResConFam' name resPars res m sp xs rs ex zs = 
    ( MkRes m resPars
    , SplicC sp rs ex zs
    , KnownSymbol name
    , AppendToTuple (Variant xs) res
    , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
    , IsTypeUniqueList name xs 
    ) 

type InvokeCon req name q m sp xs rs ex = InvokeConFam req name m sp xs rs ex (ReqResult (NamedRequest req name) (VWrap xs NamedTuple))
type family InvokeConFam req name m sp xs rs ex zs where
  InvokeConFam req name m sp xs rs ex zs =
    ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
    , Show req
    , KnownSymbol name
    , SplicC sp rs ex zs
    )

type ClearCon name q m sp xs rs ex = ClearConFam name m sp xs rs ex (ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr))
type family ClearConFam name m sp xs rs ex zs where
  ClearConFam name m sp xs rs ex zs =
    ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
    , KnownSymbol name
    , SplicC sp rs ex zs
    )

type ForeverCon f q m sp xs rs ex = 
  ( Interpretable q m sp xs xs ex f 
  , rs ~ '[]
  )

type NextEvCon q m sp xs rs ex = NextEvConFam q m sp xs rs ex (NextStates (TransWrap xs))
type family NextEvConFam q m sp xs rs ex zs where
  NextEvConFam q m sp xs rs ex zs =
    ( Transition m (TransWrap xs) 
    , SplicC sp rs ex zs
    , q ~ (ContT Bool)
    )

type NextStepsCon steps q m sp xs rs ex = 
  ( q ~ ContT Bool
  , NextSteps steps m sp xs rs ex (StepsFuncFam steps GetNextAllFunc)
  )

type SkipCon q m sp xs rs ex = SkipConFam q m sp xs rs ex (TotalSteps sp xs GetNextAllFunc)
type family SkipConFam q m sp xs rs ex steps where
  SkipConFam q m sp xs rs ex steps =
    ( q ~ ContT Bool
    , NextSteps steps m sp xs rs ex (StepsFuncFam steps GetNextAllFunc)
    , Eval (SkipFunc sp xs) ~ '(rs,ex)
    )

{-    
type instance STransCon OpFunc  = OpCon 
data OpCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (OpCon q m sp xs rs1 ex1) = ()

type instance STransCon LiftIOFunc  = LiftIOCon 
data LiftIOCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (LiftIOCon q m sp xs rs1 ex1) = MonadIO m

type instance STransCon NoopFunc  = NoopCon 
data NoopCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (NoopCon q m sp xs rs1 ex1) = ()

type instance STransCon WhatNextFunc  = WhatNextCon 
data WhatNextCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (WhatNextCon q m sp xs rs1 ex1) = ()

type instance STransCon QueryStateFunc  = QueryStateCon 
data QueryStateCon :: ((* -> *) -> * -> *) -> (* -> *) -> * -> [*] -> [*] -> [*] -> Exp Constraint 
type instance Eval (QueryStateCon q m sp xs rs1 ex1) = ()
-}

--

class Interpretable q m sp xs rs ex f  | f sp xs -> rs ex where
  interpret :: STransData m sp f a -> STrans q m sp xs rs ex f a  

type Qm q m =   
  ( MonadTrans q
  , Monad (q m)
  , Monad m
  )

instance 
  ( Qm q m
  ) => Interpretable q m sp xs xs '[] (ReturnFunc a) where
  interpret (Return a) = returnT a

instance 
  ( Qm q m
  , NewResCon name resPars q m sp xs rs ex 
  , Eval (NewResFunc resPars name m sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex (NewResFunc resPars name m) where
  interpret (NewRes named resPars) = newRes named resPars

instance 
  ( Qm q m
  , InvokeCon req name q m sp xs rs ex
  , Eval (InvokeAllFunc req name sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex (InvokeAllFunc req name) where
  interpret (Invoke named req) = invoke named req

instance 
  ( Qm q m
  , ClearCon name q m sp xs rs ex
  , Eval (ClearAllFunc name sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex (ClearAllFunc name) where
  interpret (Clear named) = clear named

instance 
  ( Qm q m
  , NextEvCon q m sp xs rs ex
  , Eval (GetNextAllFunc sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex GetNextAllFunc where
  interpret NextEv' = nextEv' 


instance 
  ( Qm q m
  , KnownNat (Length ex1)
  , Interpretable q m sp xs rs1 ex1 f1 
  , Interpretable q m sp rs1 rs ex2 f2 
  , Concat ex1 ex2 ~ ex
  ) => Interpretable q m sp xs rs ex (ComposeFunc f1 f2) where
    interpret (Compose sd1 sd2) = composeT (interpret sd1) (interpret sd2) 

instance   
  ( Qm q m
  , KnownNat (Length ex1)
  , Interpretable q m sp xs rs1 ex1 f1 
  , Interpretable q m sp rs1 rs ex2 f2 
  , Concat ex1 ex2 ~ ex
  ) => Interpretable q m sp xs rs ex (BindFunc f1 f2) where
    interpret (Bind sd1 f_sd2) = bindT (interpret sd1) (\a -> interpret (f_sd2 a)) 

    
instance  
  ( Qm q m
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , ListSplitter sp1 xs
  , '(xs_sub, ex_sub) ~ ListSplitterRes2 sp1 xs
  , rs1 ~ Union rs_sub ex_sub  
  , Interpretable q m sp xs_sub rs_sub ex f_sub 
  , GetInstance sp1
  ) => Interpretable q m sp xs rs1 ex (CaptureFunc sp1 f_sub) where
    interpret (On sd) = capture getInstance (interpret sd)
  

instance  
  ( Qm q m
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs1 ex1 zs
  , Interpretable q m (sp :&& sp1) xs_sub rs_sub ex f_sub 
  , GetInstance sp1
  ) => Interpretable q m sp xs rs1 ex1 (EmbedFunc sp1 f_sub) where
    interpret (Try sd) = embed getInstance (interpret sd)

instance  
  ( Qm q m
  , NextStepsCon steps q m sp xs rs ex
  , Eval (NextStepsFunc steps sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex (NextStepsFunc steps) where
    interpret (NextSteps px) = nextSteps' px 
        
instance  
  ( Qm q m
  , SkipCon q m sp xs rs ex
  , Eval (SkipFunc sp xs) ~ '(rs,ex)
  ) => Interpretable q m sp xs rs ex SkipFunc where
    interpret Skip = skipT 
            
instance  
  ( Qm q m
  , ForeverCon f q m sp xs rs ex
  ) => Interpretable q m sp xs rs ex (ForeverFunc f) where
    interpret (Forever sd) = forever (interpret sd) 
              
instance  
  ( Qm q m
  , TermState m (ClrVar xs)
  , rs ~ '[()]
  , ex ~ '[]
  ) => Interpretable q m sp xs rs ex ClearAllVarFunc where
    interpret ClearResources' = termAndClearAllResources 
                

instance  
  ( Qm q m
  , GetTypeByNameVar name x xs
  ) => Interpretable q m sp xs xs '[] (OpResFunc name x) where
    interpret (OpRes named getter) = opRes named getter 

instance  
  ( Qm q m
  ) => Interpretable q m sp xs xs '[] NoopFunc where
    interpret Noop = noop 
      
instance  
  ( Qm q m
  , MonadIO m
  ) => Interpretable q m sp xs xs '[] LiftIOFunc where
    interpret (LiftIO ioa) = liftIO ioa 
        
        
{-  
instance 
  ( MonadTrans q
  , Monad (q m)
  , Monad m
  , '(rs,ex) ~ Eval (f sp xs) 
  , Eval ((STransCon f) q m sp xs rs ex)
  ) => Interpretable q m sp xs rs ex f where   
    interpret (Return a) = returnT a
    interpret (NewRes named resPars) = newRes named resPars
    interpret (Invoke named req) = invoke named req
    interpret (Clear named) = clear named
    interpret (NextSteps px) = nextSteps' px 
    interpret (Op ma) = op ma 
    interpret (OpRes named getter) = opRes named getter 
    interpret (Compose sd1 sd2) = composeData sd1 sd2 
    interpret (Bind sd1 f_sd2) = bindData sd1 f_sd2 
    interpret NextEv' = nextEv' 
    interpret ClearResources' = termAndClearAllResources 
    interpret Noop = noop
    interpret (LiftIO ioa) = liftIO ioa
    interpret (Forever sd) = forever (interpret sd) 
    interpret (Try sd) = embedData getInstance sd 
    interpret (On sd) = captureData getInstance sd 
    interpret Skip = skipT
-}

