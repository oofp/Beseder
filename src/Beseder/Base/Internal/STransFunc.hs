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
{-# LANGUAGE AllowAmbiguousTypes #-}

module Beseder.Base.Internal.STransFunc where

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
import           Beseder.Base.Internal.Core
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.Named
import           Beseder.Base.Internal.Classes
import           Beseder.Base.Internal.TypeExp
import           Beseder.Base.Internal.TupleHelper
import           Beseder.Base.Internal.SplitOps
import           Beseder.Base.Internal.STrans
import           Beseder.Utils.ListHelper
import           Beseder.Utils.VariantHelper
import           Beseder.Base.Internal.SplitFlow
import           Beseder.Utils.Lst

class ToTrans (funcData :: * -> [*] -> Exp ([*],[*])) dict q m sp (xs :: [*]) a where
  toTrans :: Proxy funcData -> Proxy dict -> Proxy m -> Proxy sp -> STrans q m sp xs (Eval (funcData sp xs)) funcData a

newtype STransPar q m sp xs rs_ex func a b = STransPar {getTrans :: b -> STrans q m sp xs rs_ex func a}  
class ToTransPar (funcData :: * -> [*] -> Exp ([*],[*])) dict q m sp (xs :: [*]) a b where
  toTransPar :: Proxy funcData -> Proxy dict -> Proxy m -> Proxy sp -> STransPar q m sp xs (Eval (funcData sp xs)) funcData a b

instance 
  ( Request m (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , zs ~ ReqResult (NamedRequest TerminateRes name) (VWrap xs NamedTupleClr)
  , KnownSymbol name
  , SplicC sp rs ex zs
  ) => ToTrans (ClearAllFunc (name::Symbol)) dict q m sp xs () where
  toTrans _ _ _ _ =
    let named :: Named name
        named = Named 
     in ClearAllTrans named

instance 
  ( Request m (NamedRequest req name) (VWrap xs NamedTuple)
  , Show req
  , KnownSymbol name
  , zs ~ ReqResult (NamedRequest req name) (VWrap xs NamedTuple)
  , SplicC sp rs ex zs
  , GetInstance req
  ) => ToTrans (InvokeAllFunc req (name :: Symbol)) dict q m sp xs () where 
  toTrans _ _ _ _ =
    let named :: Named name
        named = Named 
    in InvokeAllTrans named getInstance

instance 
  ( MkRes m resPars
  , res ~ St (ResSt resPars m) name
  , zs ~ AppendToTupleList xs res
  , SplicC sp rs ex zs
  , Show resPars
  , KnownSymbol name
  , AppendToTuple (Variant xs) res
  , AppendToTupleResult (Variant xs) res ~ Variant (AppendToTupleList xs res)  
  , IsTypeUniqueList name xs 
  , GetInstance resPars
  ) => ToTrans (NewResFunc resPars name m) dict q m sp xs () where
  toTrans _ _ _ _ =     
    let named :: Named name
        named = Named 
    in NewResTrans named getInstance

instance 
  ( Transition m (TransWrap xs) 
  , SplicC sp rs ex zs
  , zs ~ NextStates (TransWrap xs)
  ) => ToTrans GetNextAllFunc dict (ContT Bool) m sp xs () where
  toTrans _ _ _ _ =  GetNextAllTrans   
       
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
  toTrans _ px_dict px_m px_sp =   
    let px_a :: Proxy f_a
        px_a = Proxy  
        px_b :: Proxy f_b
        px_b = Proxy  
    in ComposeTrans (toTrans px_a px_dict px_m px_sp) (toTrans px_b px_dict px_m px_sp)     

data AdHocFunc :: dict -> Symbol -> * -> [*] -> Exp ([*],[*])
type instance Eval (AdHocFunc dict funcName sp xs) = '(xs, '[])  

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
    toTrans _ px_dict px_m px_sp =   
      let px_a :: Proxy f_a
          px_a = Proxy  
          px_b :: Proxy f_b
          px_b = Proxy
          transA :: STrans q m sp as '(rs1, ex1) f_a a  
          transA = toTrans px_a px_dict px_m px_sp
          transB :: STransPar q m sp rs1 '(rs2, ex2)  f_b b a
          transB = toTransPar px_b px_dict px_m px_sp
      in BindTrans transA (getTrans transB)     
