
module Beseder.Base.Control
    ( newRes
    , invoke
    , nextEv
    , nextEv'
    , newState
    , clear
    , forever
    , while
    , try
    , reach
    , on
    , iff
    , ifElse
    , gets
    , opRes
    , op
    , termAndClearAllResources 
    , clearAllResources
    , handleEvents
    , pumpEvents
    --, extendForLoop
    , noop
    , order
    , whatNext
    , (>>)
    , (>>=)
    , return
    , liftIO
    , skipAll
    , skipTo
    , All
    , By (..)
    , Not (..)
    , (:&&)
    , (:||)
    , (:?)
    , Dynamics
    , Statics
    , NoSplitter
    , module Beseder.Base.Internal.STransDef
    , STrans (..)
    , STransApp (..)
    , Has 
    , NatOne (..)
    , execTrans
    , execApp
    ) where


import Beseder.Base.Internal.STransIxDo 
--import Beseder.Base.Internal.STransIx
import Beseder.Base.Internal.STransDef
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.NatOne
