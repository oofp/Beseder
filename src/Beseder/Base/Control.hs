
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
    , module Beseder.Base.Internal.STransData
    , module Beseder.Base.Internal.STransDataDo 
    , module Beseder.Base.Internal.STransMonad
    , STrans (..)
    , STransApp (..)
    , Has 
    , NatOne (..)
    , execTrans
    , execApp
    , interpret
    ) where


import Beseder.Base.Internal.STransIxDo 
import Beseder.Base.Internal.STransDef
import Beseder.Base.Internal.STransMonad
import Beseder.Base.Internal.STransData
import Beseder.Base.Internal.STransDataIntrp
import qualified Beseder.Base.Internal.STransDataDo
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.NatOne