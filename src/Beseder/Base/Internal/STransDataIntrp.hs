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
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Beseder.Base.Internal.STransDataIntrp 
  ( interpret
  , Interpretable 
  ) where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on, liftIO, gets)
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
import           Beseder.Base.Internal.STransFunc
import           Beseder.Base.Internal.STransData 
import           Beseder.Utils.VariantHelper
import           Beseder.Utils.ListHelper

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
  , f2 ~ Eval (fd xs rs1)
  , ReifyTrans m sp f2 () 
  -- , Eval (f2 sp rs1) ~ '(rs,ex2)
  , Interpretable q m sp rs1 rs ex2 f2 
  , Concat ex1 ex2 ~ ex
  ) => Interpretable q m sp xs rs ex (ScopeFunc f1 fd) where
    interpret (Scope sd1 px) = scopeT (interpret sd1) (interpret (reifyTrans (Proxy @f2))) 

instance 
  ( Qm q m
  , Interpretable q m sp xs rs ex f 
  , f ~ Eval (ff xs)
  , ReifyTrans m sp f () 
  ) => Interpretable q m sp xs rs ex (FuncFunc ff) where
    interpret (Func px) = funcT (interpret (reifyTrans (Proxy @f))) 
    
    
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
  , ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs ~ Union rs_sub1 rs_sub2  
  , Liftable rs_sub1 rs
  , Liftable rs_sub2 rs
  , ex ~ Union ex1 ex2  
  , Liftable ex1 ex
  , Liftable ex2 ex
  , Monad (q m)
  , MonadTrans q
  , GetInstance sp1
  , Interpretable q m sp xs_sub rs_sub1 ex1 f_sub1 
  , Interpretable q m sp ex_sub rs_sub2 ex2 f_sub2 
  ) => Interpretable q m sp xs rs ex (CaptureOrElseFunc sp1 f_sub1 f_sub2) where
    interpret (OnOrElse sd1 sd2) = captureOrElse (getInstance @sp1) (interpret sd1) (interpret sd2)
  
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
  , Interpretable q m sp xs rs ex f 
  ) => Interpretable q m sp xs rs ex (BlockFunc l f) where
    interpret (Block l sd) = block (interpret sd) 
      
instance  
  ( Qm q m
  , ForeverCon f q m sp xs rs ex
  ) => Interpretable q m sp xs rs ex (ForeverFunc f) where
    interpret (Forever sd) = forever (interpret sd) 

instance  
  ( Qm q m
  , Interpretable q m sp xs xs ex f
  ) => Interpretable q m sp xs xs ex (WhileFunc f) where
    interpret (While sd) = while (interpret sd) 

instance  
  ( Qm q m
  , Interpretable q m sp xs xs1 ex f
  , Eval (f sp xs) ~ '(xs1,ex)
  , xs2 ~ FilterList xs xs1
  , xs3 ~ FilterList xs2 xs1
  , VariantSplitter xs2 xs3 xs1
  , Liftable xs3 xs
  ) => Interpretable q m sp xs xs2 ex (GetNewStateFunc f) where
    interpret (NewState sd) = newState (interpret sd) 

instance  
  ( Qm q m
  , loopRes ~ First (TransformLoop sp xs f)
  , Eval (f sp loopRes) ~ '(rs,ex)
  , Interpretable q m sp loopRes rs ex f
  , Liftable rs loopRes 
  , Liftable xs loopRes
  ) => Interpretable q m sp xs ('[]) ex (HandleLoopFunc f) where
    interpret (HandleLoop sd) = handleLoop (interpret sd)


instance  
  ( Qm q m
  , ex ~ Union ex1 ex2 
  , Liftable ex1 ex
  , Liftable ex2 ex
  , rs ~ Union rs1 rs2
  , Liftable rs1 rs
  , Liftable rs2 rs
  , Interpretable q m sp xs rs1 ex1 f1
  , Interpretable q m sp xs rs2 ex2 f2
  ) => Interpretable q m sp xs rs ex (IfElseFunc f1 f2) where
    interpret (IfElse fl sd1 sd2) = ifElse fl (interpret sd1) (interpret sd2)

instance  
  ( Qm q m
  , rs ~ Union rs1 xs
  , Liftable rs1 rs
  , Liftable xs rs  
  , Interpretable q m sp xs rs1 ex f1
  ) => Interpretable  q m sp xs rs ex (IffFunc f1 ) where
    interpret (Iff fl sd1) = iff fl (interpret sd1) 
      
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
  ) => Interpretable q m sp xs xs '[] (OpFunc a) where
    interpret (Op ma) = op ma 
  
  
instance  
  ( Qm q m
  , x ~ St st name
  , GetTypeByNameVar name x xs
  ) => Interpretable q m sp xs xs '[] (GetFunc name x) where
    interpret (Gets named getter) = gets named getter 

instance  
  ( Qm q m
  ) => Interpretable q m sp xs xs '[] NoopFunc where
    interpret Noop = noop 

instance  
  ( Qm q m
  , '(xs, '[]) ~ ListSplitterRes2 sp1 xs 
  ) => Interpretable q m sp xs xs '[] (AssertFunc sp1) where
    interpret Assert = assert 
  
  
instance  
  ( Qm q m
  ) => Interpretable q m sp xs xs '[] (LabelFunc named) where
    interpret (Label named) = label named 
        
      
instance  
  ( Qm q m
  , MonadIO m
  ) => Interpretable q m sp xs xs '[] LiftIOFunc where
    interpret (LiftIO ioa) = liftIO ioa 
        
{-    
instance  
  ( Qm q m
  , s ~ Proxy xs
  ) => Interpretable q m sp xs xs '[] (WhatNextFunc s) where
    interpret WhatNext = whatNext 
        
instance  
  ( Qm q m
  , pxNames ~ Proxy (GetAllNames xs)
  ) => Interpretable q m sp xs xs '[] (WhatNamesFunc pxNames) where
    interpret WhatNames = whatNames 
-}


