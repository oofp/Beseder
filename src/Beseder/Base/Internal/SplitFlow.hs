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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures      #-}

module Beseder.Base.Internal.SplitFlow where

import           Protolude                    hiding (Product, handle)
import           Haskus.Utils.Flow
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant
import           Beseder.Base.Internal.Flow
import           Beseder.Base.Internal.SplitOps
import           Beseder.Utils.VariantHelper
import           Beseder.Utils.ListHelper
import           Beseder.Utils.Lst
   
splitFlow :: 
  ( ListSplitter sp ys
  , ListSplitterRes sp ys ~ xs
  , FilterList xs ys ~ ex
  , VariantSplitter xs ex ys
  , Monad (q m)
  ) => sp -> MFlow q m ys -> MFlowEx q m xs ex 
splitFlow sp flow_ys = do
  v_ys <- flow_ys
  let ol_new = doSplit sp (lstFromVar v_ys)
      either_xs_ex = splitVar1' v_ys (pxFromLst ol_new)
  case either_xs_ex of 
    Right v_ex -> return $ Left $ v_ex
    Left v_xs -> return $ Right v_xs

splitV :: 
  ( ListSplitter sp ys
  , ListSplitterRes sp ys ~ xs
  , FilterList xs ys ~ ex
  , VariantSplitter xs ex ys
  ) => sp -> V ys -> Either (V ex) (V xs)  
splitV sp v_ys = do
  let ol_new = doSplit sp (lstFromVar v_ys)
      either_xs_ex = splitVar1' v_ys (pxFromLst ol_new)
  case either_xs_ex of 
    Right v_ex -> Left v_ex
    Left v_xs -> Right v_xs
  
rearrangeFlow :: 
  ( ys ~ Union xs1 ex1
  , Liftable xs1 ys 
  , Liftable ex1 ys
  , ListSplitter sp ys
  , ListSplitterRes sp ys ~ xs
  , FilterList xs ys ~ ex
  , VariantSplitter xs ex ys
  , Monad (q m)
  ) => sp -> MFlowEx q m xs1 ex1 -> MFlowEx q m xs ex 
rearrangeFlow sp flow1 = splitFlow sp (eitherFlow flow1)    

eitherFlow :: 
  ( xs_ex ~ Union xs ex 
  , Liftable xs xs_ex 
  , Liftable ex xs_ex
  , Functor (q m) 
  ) => MFlowEx q m xs ex -> MFlow q m xs_ex  
eitherFlow = fmap (either liftVariant liftVariant)  

eitherFlow' :: 
  ( xs_ex ~ Union xs ex 
  , Liftable xs xs_ex 
  , Liftable ex xs_ex
  , Functor (q m) 
  ) => MFlowExA q m xs ex () -> MFlow q m xs_ex  
eitherFlow' = fmap (either liftVariant (liftVariant . fst))  

toExFlow :: Functor (q m) => MFlow q m xs -> MFlowEx q m xs '[]
toExFlow = fmap Right

toExFlowA :: Monad (q m) => MFlowA q m xs a -> MFlowExA q m xs '[] a
toExFlowA = fmap Right

fromExFlow :: (Monad (q m)) => MFlowEx q m xs '[] -> MFlow q m xs 
fromExFlow flowEx = do
  ei <- flowEx
  case ei of 
    Left _ -> undefined
    Right res -> return res 

fromExFlowA :: (Monad (q m)) => MFlowExA q m xs '[] a -> MFlowA q m xs a 
fromExFlowA flowEx = do
  ei <- flowEx
  case ei of 
    Left _ -> undefined
    Right res -> return res 

fromExFlowA' :: (Monad (q m)) => MFlowExA q m xs '[] () -> MFlow q m xs 
fromExFlowA' flowEx = do
  ei <- flowEx
  case ei of 
    Left _ -> undefined
    Right (res,()) -> return res 

lstFromVar :: V xs -> Lst xs
lstFromVar _ = Lst

pxFromLst :: Lst xs -> Proxy xs
pxFromLst _ = Proxy

    