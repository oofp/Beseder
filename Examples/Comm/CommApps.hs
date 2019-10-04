{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  CommApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Control.Monad.Cont (ContT)                                               
import           Beseder.Base.Control                                               
import           Beseder.Resources.Comm 
import           Beseder.Resources.State.DataRes
--import           Data.String 

proxyApp :: forall m i1 i2 o1 o2 e1 e2 comm1 comm2. 
  ( MonadIO m
  , CommProv comm1 i1 o1 e1 m  
  , CommProv comm2 i2 o2 e2 m 
  , Show o2
  , Show o1
  , Show comm1
  , Show comm2
  ) => CommRes comm1 i1 o1 e1 
      -> CommRes comm2 i2 o2 e2 
      -> (i1 -> o2) -> (i2 -> o1) -> (i1 -> Bool) 
      -> STransApp (ContT Bool) m NoSplitter '[()] '[()] '[] ()
proxyApp comRes1 comRes2 i1o2 i2o1 contPred = MkApp $ do
  newRes #com1 comRes1
  newRes #com2 comRes2
  newRes #flCont initAsTrue
  try @(("com1" :? IsCommAlive) :&& ("com2" :? IsCommAlive) :&& ("flCont" :? IsTrue)) $ 
    handleEvents $ do
      on @("com1" :? IsMessageReceived) $ do  
        msgRcvd1 <- gets #com1 getIncomingMsg
        iff (contPred msgRcvd1) (invoke #flCont setFalse)
        on @("com2" :? IsCommConnected) $ do  
          let o2 = i1o2 msgRcvd1 
          invoke #com2 (SendMsg o2)
        invoke #com1 GetNextMsg
      on @("com2" :? IsMessageReceived) $ do  
        on @("com1" :? IsCommConnected) $ do  
          msgRcvd2 <- gets #com2 getIncomingMsg
          let o1 = i2o1 msgRcvd2 
          invoke #com1 (SendMsg o1)
        invoke #com2 GetNextMsg
  termAndClearAllResources


