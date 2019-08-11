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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax      #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Beseder.Base.Internal.STransFunc where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                                (>>), (>>=), forever, until,try,on, First)
import           Control.Monad.Cont (ContT)
import           Control.Monad.Identity (IdentityT)
import           Haskus.Utils.Flow
import           Data.Text
import           Data.Typeable
import           GHC.TypeLits
import           Haskus.Utils.Tuple
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STrans
import           Beseder.Base.Internal.STransDo
import           qualified Beseder.Base.Internal.STransDo as SDo
import           Beseder.Utils.ListHelper
import           Beseder.Utils.VariantHelper
import           Beseder.Base.Internal.SplitFlow
import           Beseder.Utils.Lst
import           Type.Errors hiding (Eval,Exp)
import           Beseder.Resources.State.StateLogger 


class ToTrans (funcData :: * -> [*] -> Exp ([*],[*])) dict q m sp (xs :: [*]) a where
  reifyTrans :: Proxy funcData -> Proxy dict -> STrans q m sp xs (Eval (funcData sp xs)) funcData a

newtype STransPar q m sp xs rs_ex func a b = STransPar {getTrans :: b -> STrans q m sp xs rs_ex func a}  
class ToTransPar (funcData :: * -> [*] -> Exp ([*],[*])) dict q m sp (xs :: [*]) a b where
  reifyTransPar :: Proxy funcData -> Proxy dict -> STransPar q m sp xs (Eval (funcData sp xs)) funcData a b

instance 
  ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , KnownSymbol name
  , SplicC sp rs ex zs
  ) => ToTrans (ClearAllFunc (name::Symbol)) dict q m sp xs () where
  reifyTrans _ _ = ClearAllTrans (Named @name)   

instance 
  ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
  , Show req
  , KnownSymbol name
  , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  , WhenStuck (ReqResult (NamedRequest req name) (VWrap xs NamedTuple)) (DelayError ('Text "No request supported detected"))
  , SplicC sp rs ex zs
  , GetInstance req
  ) => ToTrans (InvokeAllFunc req (name :: Symbol)) dict q m sp xs () where 
  reifyTrans _ _ = InvokeAllTrans (Named @name) getInstance   

instance 
  ( MkRes m resPars
  , res ~ St (ResSt m resPars) name
  , zs ~ AppendToTupleList xs res
  , SplicC sp rs ex zs
  , Show resPars
  , KnownSymbol name
  , AppendToTuple (Variant xs) res
  , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
  , IsTypeUniqueList name xs 
  , GetInstance resPars
  ) => ToTrans (NewResFunc resPars name m) dict q m sp xs () where
  reifyTrans _ _ = NewResTrans (Named @name) getInstance   

instance 
  ( Transition m (TransWrap xs) 
  , SplicC sp rs ex zs
  , zs ~ NextStates (TransWrap xs)
  ) => ToTrans GetNextAllFunc dict (ContT Bool) m sp xs () where
  reifyTrans _ _ = GetNextAllTrans   
       
instance 
  ( ex_un ~ Union ex1 ex2
  , Liftable ex1 ex_un
  , Liftable ex2 ex_un
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , Composer q m sp as rs1 ex1 f_a rs2 ex2 f_b ex_un b 
  , ToTrans f_a dict q m sp as ()
  , ToTrans f_b dict q m sp rs1 b
  , ComposeFam' (IsIDFunc f_a) f_a f_b sp as ~ '(rs2,ex_un)
  ) => ToTrans (ComposeFunc f_a f_b) dict q m sp as b where 
  reifyTrans _ _ = 
    let t1 :: STrans q m sp as (Eval (f_a sp as)) f_a ()
        t1 = reifyTrans (Proxy @f_a) (Proxy @dict) 
        t2 :: STrans q m sp rs1 (Eval (f_b sp rs1)) f_b b
        t2 = reifyTrans (Proxy @f_b) (Proxy @dict)
    in ComposeTrans t1 t2  

instance 
  ( --Eval (func sp xs) ~ '(xs, '[]) 
     TransDict q m dict keyName a
  ) => ToTrans (DictFunc keyName) dict q m sp xs a where 
  reifyTrans _ _ = DictTrans (Proxy @dict) (Named @keyName)  

instance 
  ( ex_un ~ Union ex1 ex2 
  , Liftable ex1 ex_un
  , Liftable ex2 ex_un
  , Eval (f_a sp as) ~ '(rs1, ex1)  --assert 
  , Eval (f_b sp rs1) ~ '(rs2, ex2) --assert
  , Binder q m sp as rs1 ex1 f_a a rs2 ex2 f_b ex_un b
  , ToTrans f_a dict q m sp as a
  , ToTransPar f_b dict q m sp rs1 b a
  , ComposeFam' (IsIDFunc f_a) f_a f_b sp as ~ '(rs2,ex_un)
  ) => ToTrans (BindFunc f_a f_b) dict q m sp as b where 
    reifyTrans _ _ =   
      let
        transA :: STrans q m sp as '(rs1, ex1) f_a a  
        transA = reifyTrans (Proxy @f_a) (Proxy @dict)  
        transB :: STransPar q m sp rs1 '(rs2, ex2)  f_b b a
        transB = reifyTransPar (Proxy @f_b) (Proxy @dict)
      in BindTrans transA (getTrans transB)     
  
instance 
  ( ListSplitter sp1 xs
  , xs_sub ~ ListSplitterRes sp1 xs
  , ex_sub ~ FilterList xs_sub xs 
  , VariantSplitter xs_sub ex_sub xs
  , rs1 ~ Union rs_sub ex_sub  
  , Liftable rs_sub rs1
  , Liftable ex_sub rs1
  , GetInstance sp1
  , ToTrans f_sub dict q m sp xs_sub ()
  , Eval (f_sub sp xs_sub) ~ '(rs_sub, ex) -- assert
  ) => ToTrans (CaptureFunc sp1 f_sub) dict q m sp xs () where  
    reifyTrans _ _  =
      let transSub :: STrans q m sp xs_sub '(rs_sub, ex) f_sub ()  
          transSub = reifyTrans (Proxy @f_sub) (Proxy @dict)
      in CaptureTrans getInstance transSub  
          

instance       
  ( sp2 ~ (sp :&& sp1) 
  , SplicC sp1 xs_sub ex_sub xs
  , zs ~ Union rs_sub (Union ex_sub ex)
  , Liftable ex zs
  , Liftable ex_sub zs
  , Liftable rs_sub zs
  , SplicC sp rs ex1 zs
  , '(rs,ex1) ~ ListSplitterRes2 sp zs
  , GetInstance sp1
  , ToTrans f_sub dict q m sp2 (ListSplitterRes sp1 xs) ()
  , Eval (f_sub (sp :&& sp1) xs_sub) ~ '(rs_sub, ex) --assert
  , ListSplitterRes sp (UnionTuple (UnionRs (Eval (f_sub (sp :&& sp1) xs_sub)) ex_sub)) ~ rs
  , FilterList rs (UnionTuple (UnionRs (Eval (f_sub (sp :&& sp1) xs_sub)) ex_sub)) ~ ex1
  ) => ToTrans (EmbedFunc sp1 f_sub) dict q m sp xs () where 
    reifyTrans _ _  =
      let transSub :: STrans q m (sp :&& sp1) xs_sub '(rs_sub, ex) f_sub ()  
          transSub = reifyTrans (Proxy @f_sub) (Proxy @dict)
      in EmbedTrans getInstance transSub  
    
instance       
  ( Eval (f sp xs) ~ '(xs,ex)
  , ToTrans f dict q m sp xs ()
  ) => ToTrans (ForeverFunc f) dict q m sp xs () where
    reifyTrans _ _  =
      let transBody :: STrans q m sp xs '(xs, ex) f ()    
          transBody = reifyTrans (Proxy @f) (Proxy @dict)
      in ForeverTrans transBody  
    
instance 
  ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
  , Show req
  , KnownSymbol name
  , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  -- , WhenStuck (ReqResult (NamedRequest req name) (VWrap xs NamedTuple)) (DelayError ('Text "No request supported detected"))
  , SplicC sp rs ex zs
  ) => ToTransPar (InvokeAllFunc req (name :: Symbol)) dict q m sp xs () req where 
  reifyTransPar _ _  = STransPar (InvokeAllTrans (Named @name))
    
instance 
  ( Liftable xs rs
  ) => ToTrans (ConstFunc rs) dict q m sp xs () where
    reifyTrans _ _  = ExtendTo (Proxy @rs)

instance 
  ( Liftable xs rs
  , rs ~ First (TransformLoop sp xs f)
  , Eval (f sp xs) ~ '(bs, ex)
  , ToTrans f dict q m sp xs ()
  ) => ToTrans (ExtendForLoopFunc f) dict q m sp xs () where
    reifyTrans _ _  =
      let transBody :: STrans q m sp xs '(bs,ex) f ()      
          transBody = reifyTrans (Proxy @f) (Proxy @dict)
      in ExtendForLoop transBody  
  
instance 
  ( Liftable rs xs
  , ToTrans f dict q m sp xs ()
  , '(rs, ex) ~ (Eval (f sp xs))
  ) => ToTrans (AlignFunc f) dict q m sp xs () where
    reifyTrans _ _  =
      let transBody :: STrans q m sp xs '(rs, ex) f ()      
          transBody = reifyTrans (Proxy @f) (Proxy @dict)
      in AlignTrans transBody  
--
buildTrans :: forall d f q m sp xs a. ToTrans f d q m sp xs a => STrans q m sp xs (Eval (f sp xs)) f a
buildTrans = reifyTrans (Proxy @f) (Proxy @d)

-- aliases
type On sp1 f_sub = CaptureFunc sp1 f_sub
type Try sp1 f_sub = EmbedFunc sp1 f_sub
type Next = On Dynamics GetNextAllFunc
type Invoke name req = InvokeAllFunc req name 
type name :-> req = InvokeAllFunc req name
infixr 5 :->

type NewRes name resPars m = NewResFunc resPars name m  
  
type HandleLoop f =
 Try Dynamics 
  ( ExtendForLoopFunc f 
  :>> ForeverFunc (AlignFunc f)
  ) 

type PumpEvents = HandleLoop Next 
type HandleEvents handler = HandleLoop (handler :>> Next)  

type family ClearResourcesFam (names :: [Symbol]) where
  ClearResourcesFam '[name] = ClearAllFunc name
  ClearResourcesFam (name ': rest) = ClearAllFunc name :>> ClearResourcesFam rest

-- type instance Eval (ClearResources names sp xs) = Eval ((ClearResourcesFam names) sp xs)

type ClearResources (names :: [Symbol]) = ClearResourcesFam names

data ClearResourcesExcept :: [Symbol] ->  * -> [*] -> Exp ([*],[*])
type instance Eval (ClearResourcesExcept names sp xs) = Eval (ClearResourcesFam (FilterList names (GetAllNames xs)) sp xs)

instance 
  ( ToTrans (ClearResourcesFam (FilterList names (GetAllNames xs))) dict q m sp xs()
  ) => ToTrans (ClearResourcesExcept names) dict q m sp xs () where
  reifyTrans _ _ = 
    let px_t :: Proxy (ClearResourcesFam (FilterList names (GetAllNames xs)))
        px_t = Proxy
    in AppWrapperTrans $ MkApp $ reifyTrans px_t (Proxy @dict)     
  
--
-- type LogFunc label xs = CaptureFunc(By "log") (InvokeAllFunc (LogState label xs) "log")
type LogFunc label xs = InvokeAllFunc (LogState label xs) "log"

data LoggerFunc :: Symbol ->  * -> [*] -> Exp ([*],[*])
type instance Eval (LoggerFunc label sp xs) = Eval (LogFunc label (ReqResult (NamedRequest TerminateRes "log") (VWrap xs NamedTupleClr)) sp xs)

logState :: 
  ( KnownSymbol label
  , ys ~ ReqResult (NamedRequest TerminateRes "log") (VWrap xs NamedTupleClr)
  , Request m (NamedRequest (LogState label ys) "log") (VWrap xs NamedTuple)
  , zs ~ ReqResult (NamedRequest (LogState label ys) "log") (VWrap xs NamedTuple)
  , SplicC sp rs ex zs
  ) => Named label -> STrans q m sp xs (Eval ((LogFunc label ys) sp xs)) (LogFunc label ys) ()
logState _labeled = do
  let logStateReq :: LogState label xs
      logStateReq = LogState
  invoke #log logStateReq    


instance 
  ( KnownSymbol label
  , ys ~ ReqResult (NamedRequest TerminateRes "log") (VWrap xs NamedTupleClr)
  , Request m (NamedRequest (LogState label ys) "log") (VWrap xs NamedTuple)
  , zs ~ ReqResult (NamedRequest (LogState label ys) "log") (VWrap xs NamedTuple)
  , SplicC sp rs ex zs
  ) => ToTrans (LoggerFunc label) dict q m sp xs () where
    reifyTrans _ _ = AppWrapperTrans $ MkApp $ logState (Named @label)


data PutStrLn :: Symbol ->  * -> [*] -> Exp ([*],[*])
type instance Eval (PutStrLn txt sp xs) = '(xs, '[])
instance 
  ( KnownSymbol txt
  , MonadIO m
  ) => ToTrans (PutStrLn txt) dict q m sp xs () where
    reifyTrans _ _ = AppWrapperTrans $ MkApp $ SDo.liftIO $ putStrLn (symbolVal (Proxy @txt))

type Trace label = On (By "log") (LoggerFunc label)
type FuncWithTrace m func = (NewResFunc StateLoggerRes "log" m) :>> func
type EvalTransFuncWithTrace m func  = Eval (FuncWithTrace m (func m) NoSplitter '[()])
type ClearAllResourcesButTrace = ClearResourcesExcept '["log"]

--type EvalTransFunc m func = Eval (func m NoSplitter '[()])
