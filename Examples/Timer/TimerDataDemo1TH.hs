{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerDataDemo1TH where

import           Protolude hiding (Type)                   
import           Beseder.Base.Common
import           Beseder.Base.ControlData
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           TimerDataDemo1 
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Data.List
import           Prelude (error)    
import           GHC.Exts (Any)    

timer0 = timer1 1
timer20 = timer2a 1 1

mkSTransDataType "timer20" "Timer20"   
mkSTransDataTypeAny "timer20" "Timer20Any"   
mkSTransDataTypeAny "timer1" "Timer1Any"   
mkSTransDataTypeAny "timer2a" "Timer2aAny"   

type Timer20Res = Eval ((Timer20 IO) NoSplitter '[()])
type Timer20ResAny = Eval ((Timer20Any) NoSplitter '[()])
-- :kind! Eval ((Timer1Any) NoSplitter '[()])
-- :kind! Timer20Res