
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
    , onOrElse
    , iff
    , ifElse
    , gets
    , opRes
    , op
    , opInter
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
    , AsyncApp
    , Has 
    , NatOne (..)
    , execTrans
    , execApp
    , interpret
    , Interpretable
    , STransData
    ) where


import Beseder.Base.Internal.STransIxDo 
import Beseder.Base.Internal.STransDef
import Beseder.Base.Internal.STransMonad
import Beseder.Base.Internal.STransData (STransData)
import Beseder.Base.Internal.STransDataIntrp
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.NatOne