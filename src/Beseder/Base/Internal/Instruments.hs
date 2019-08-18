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

module Beseder.Base.Internal.Instruments 
    ( varLengthInstr 
    , varIndexInstr
    ) where

import           Protolude                    hiding (Product, handle,TypeError,First)
import           Beseder.Base.Internal.STrans
import           Beseder.Base.Internal.TypeExp
import           Beseder.Utils.VariantHelper
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant

data ListLengthC :: [*] -> (* -> *) -> Exp (Constraint)
type instance Eval (ListLengthC xs m) =  KnownNat (Length xs)

varLengthInstr :: MonadIO m => Instrumentor m ListLengthC   
varLengthInstr = Instrumentor printVarLength

printVarLength :: (MonadIO m, KnownNat (Length xs)) => V xs -> m ()
printVarLength v_xs = 
    let txt :: Text
        txt = show (getVarLength v_xs)
    in liftIO $ putStrLn txt


data VarIndexC :: [*] -> (* -> *) -> Exp (Constraint)
type instance Eval (VarIndexC xs m) =  ()

varIndexInstr :: MonadIO m => Instrumentor m VarIndexC   
varIndexInstr = Instrumentor printVarIndex

printVarIndex :: (MonadIO m) => V xs -> m ()
printVarIndex v_xs = 
  let txt :: Text
      txt = show (variantIndex v_xs)
  in liftIO $ putStrLn txt
    