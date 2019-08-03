module Beseder.Base.Internal.Classes 
    ( TaskPoster (..)
    , GetInstance (..)
    ) where

import Protolude

class (MonadIO q) => TaskPoster q where
    getTaskPoster :: q (q Bool -> IO ())

class GetInstance a where
    getInstance :: a

      