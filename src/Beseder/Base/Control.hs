
module Beseder.Base.Control
    ( withRes
    , newRes
    , invoke
    , nextEv
    , clear
    , forever
    , while
    , try
    , on
    , termAndClearAllResources 
    , clearAllResources
    , handleEvents
    , pumpEvents
    , noop
    , whatNext
    , (>>)
    , (>>=)
    , return
    , liftIO
    , All
    , By (..)
    , Not (..)
    , (:&&)
    , (:||)
    , (:?)
    , Dynamics
    , Statics
    , NoSplitter
    , EvalTransFunc
    -- trans functions
    , WithResFunc 
    , WithResAllFunc 
    , NewResFunc 
    , InvokeAllFunc 
    , ClearAllFunc 
    , GetNextFunc 
    , GetNextAllFunc 
    , InvokeFunc 
    , ClearAllVarFunc
    , ComposeFunc 
    , (:>>) 
    , IffFunc
    , IfElseFunc 
    , CaptureFunc 
    , EmbedFunc 
    , IDFunc 
    , ForeverFunc 
    , AlignFunc 
    , ConstFunc
    , module Beseder.Base.Internal.STransFunc
    , STrans
    ) where


import Beseder.Base.Internal.STransDo 
import Beseder.Base.Internal.STrans
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.STransFunc
