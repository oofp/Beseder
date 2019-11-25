
module Beseder.Base.ControlData
    ( newRes
    , invoke
    , nextEv
    , nextEv'
    , clear
    , clearResources
    , try
    , on
    , onOrElse
    , opRes
    , gets
    , (>>)
    , (>>=)
    , return
    , skipAll
    , skipTo
    , noop
    --, whatNext
    --, whatNames
    , liftIO
    , op
    , forever
    , nextSteps
    , clearAllResources
    , termAndClearAllResources
    , while
    , newState 
    , handleEvents
    , pumpEvents 
    , ifElse
    , iff
    , evalSTransData
    , evalSTransDataApp
    , evalSTransData'
    , evalSTransDataApp'
    , scopeRes
    , clearAllBut
    , All
    , By (..)
    , Not (..)
    , (:&&)
    , (:||)
    , (:?)
    , Dynamics
    , Statics
    , NoSplitter
    , STransData
    , STrans
    , interpret
    , Interpretable
    , module Beseder.Base.Internal.STransDef
    , module Beseder.Base.Internal.STransProc
    , NatOne (..)
    , NatConv 
) where

import Beseder.Base.Internal.STransData
import Beseder.Base.Internal.STransDataIntrp
import Beseder.Base.Internal.STransDataDo
import Beseder.Base.Internal.STransDataCombo
import Beseder.Base.Internal.STransDef
import Beseder.Base.Internal.STransProc
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.NatOne
import Beseder.Base.Internal.STransIx (STrans)