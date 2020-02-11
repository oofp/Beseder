{-# LANGUAGE MultiParamTypeClasses #-}

module Beseder.Base.Internal.Classes 
    ( TaskPoster (..)
    , GetInstance (..)
    , CreateFrom (..)
    ) where

import Protolude

class (MonadIO q) => TaskPoster q where
    getTaskPoster :: q (q Bool -> IO ())

class GetInstance a where
    getInstance :: a

class CreateFrom a b where
    createFrom :: a -> b
