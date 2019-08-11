
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
    , iff
    , ifElse
    , gets
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
    , BindFunc 
    , (:>>) 
    , (:>>=) 
    , IffFunc
    , IfElseFunc 
    , CaptureFunc 
    , EmbedFunc 
    , IDFunc 
    , ForeverFunc 
    , AlignFunc 
    , ConstFunc
    , DictFunc
    , module Beseder.Base.Internal.STransFunc
    , STrans
    , STransApp (..)
    , applyTransApp
    , TransDict (..)
    ) where


import Beseder.Base.Internal.STransDo 
import Beseder.Base.Internal.STrans
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.STransFunc
