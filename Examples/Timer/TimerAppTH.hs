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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-} 

{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  TimerAppTH where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Control.Monad.Cont (ContT)
import           qualified Protolude 
import           TimerHandlerTH
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

type T1 = "t1"
type T2 = "t2"
type T3 = "t3"
type T4 = "t4"

timerHello :: TaskPoster m  => Int -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
timerHello timeoutSec1 = do
  liftIO $ putStrLn ("Entered timerHello"::Text)
  $(handlerQ ''T1 ''T2) timeoutSec1
  $(handlerQ ''T3 ''T4) timeoutSec1
