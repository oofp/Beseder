{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  DoorCompositeApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Beseder.Misc.Misc
import           Beseder.Resources.Composite
import           Data.String 
import           Control.Monad.Cont (ContT)
import           SelfClosingDoor

doorTrans :: Int -> Int -> STransApp (ContT Bool) TaskQ NoSplitter '[()] _ _ () 
doorTrans openTimeoutSec closedTimeoutSec = MkApp $ do
  liftIO $ putStrLn ("Entered doorApp"::Text)
  newSelfClosingDoor #dr (DoorCfg openTimeoutSec closedTimeoutSec)
  skipTo @("dr" :? IsDoorLocked)
  invokeC #dr unlockDoor 
  invokeC #dr openDoor 
  skipAll                    -- let auto close and then auto lock
  invokeC #dr unlockDoor     -- unlock
  invokeC #dr lockDoor       -- lock again
  clear #dr

-- runAsyncApp (doorTrans 4 5)
