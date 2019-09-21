{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Algebra where

import           Protolude
import           Haskus.Utils.Variant

--create resource
class MkRes ctx res  where
    type ResSt ctx res  :: *
    mkRes :: res -> ctx (ResSt ctx res)

--clear resource    
class TermState ctx state where
    terminate :: state -> ctx ()
  
--unsolicted resource state transition    
class Transition ctx state1 where
    type NextStates state1 :: [*]
    next :: state1 -> (V (NextStates state1) -> ctx Bool) -> ctx Bool
  
-- request invocation    
class Request ctx req state1 where
    type ReqResult (req :: *) (st :: *) :: [*]
    request :: req -> state1 -> ctx (V (ReqResult req state1))
  
  
  