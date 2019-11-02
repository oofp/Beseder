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
{-# LANGUAGE QuantifiedConstraints #-}

module Beseder.Base.Internal.STransMonad where

import           Protolude                    hiding (Product, handle,TypeError,First,forever, on)
import           Haskus.Utils.Types.List
import           Beseder.Base.Internal.STransDef

class STransMonad (ixm :: * -> [*] -> [*] -> [*] -> (* -> [*] -> ([*],[*]) -> *) -> * -> *) where
  type Cx ixm (sp :: *) (xs :: [*])  (rs :: [*]) (ex :: [*]) (f :: * -> [*] -> ([*],[*]) -> *) :: Constraint

  st_return :: a -> ixm sp xs xs ('[]) (ReturnFunc a) a
  st_compose :: 
    ( --KnownNat (Length ex)
      Cx ixm sp xs rs ex f1
    , Cx ixm sp rs rs1 ex1 f2
    ) => ixm sp xs rs ex f1 () -> ixm sp rs rs1 ex1 f2 b -> ixm sp xs rs1 (Concat ex ex1) (ComposeFunc f1 f2) b  
  st_bind :: 
    ( --KnownNat (Length ex)
      Cx ixm sp xs rs ex f1
    , Cx ixm sp rs rs1 ex1 f2
    ) => ixm sp xs rs ex f1 a -> (a -> ixm sp rs rs1 ex1 f2 b) -> ixm sp xs rs1 (Concat ex ex1) (BindFunc f1 f2) b  

return :: STransMonad ixm => a -> ixm sp xs xs '[] (ReturnFunc a) a
return = st_return

(>>=) :: 
  ( STransMonad ixm
  -- , KnownNat (Length ex1)
  , Cx ixm sp xs rs1 ex1 f1
  , Cx ixm sp rs1 rs2 ex2 f2
  ) => ixm sp xs rs1 ex1 f1 a -> (a -> ixm sp rs1 rs2 ex2 f2 b) -> ixm sp xs rs2 (Concat ex1 ex2) (BindFunc f1 f2) b
(>>=) = st_bind

(>>) :: 
  ( STransMonad ixm
  -- , KnownNat (Length ex1)
  , Cx ixm sp xs rs1 ex1 f1
  , Cx ixm sp rs1 rs2 ex2 f2
  ) => ixm sp xs rs1 ex1 f1 () -> ixm sp rs1 rs2 ex2 f2 b -> ixm sp xs rs2 (Concat ex1 ex2) (ComposeFunc f1 f2) b
(>>) = st_compose
