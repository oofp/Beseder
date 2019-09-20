
module Beseder.Base.Control
    ( withRes
    , newRes
    , invoke
    , nextEv
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
    , getM
    , termAndClearAllResources 
    , clearAllResources
    , handleEvents
    , pumpEvents
    , extendForLoop
    , noop
    , whatNext
    , whatSplitter
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
    , IfJustFunc
    , CaptureFunc 
    , EmbedFunc 
    , IDFunc 
    , ForeverFunc 
    , AlignFunc 
    , ConstFunc
    , DictFunc
    , AskFunc
    , AsksFunc
    , MapFunc
    , module Beseder.Base.Internal.STransFunc
    , module Beseder.Base.Internal.Instruments
    , STrans
    , STransApp (..)
    , applyTransApp
    , instrumentTrans
    , Instrumentor (..)
    , TransDict (..)
    , Has 
    ) where


import Beseder.Base.Internal.STransDo 
import Beseder.Base.Internal.STrans
import Beseder.Base.Internal.SplitOps
import Beseder.Base.Internal.STransFunc
import Beseder.Base.Internal.STransInstr
import Beseder.Base.Internal.Instruments
