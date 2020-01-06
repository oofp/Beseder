{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}

module Beseder.Resources.Comm.CommProv 
  ( CommRes (..)
  , SendMsg (..)
  , GetNextMsg (..)
  , CloseComm (..)
  , CommProv (..)
  , CommInitiated 
  , CommWaitForMsg 
  , CommMsgRcvd 
  , CommClosed 
  , CommFailed 
  , IsMessageReceived
  , IsCommAlive 
  , IsCommConnected 
  , getCommFailure
  , getIncomingMsg
  ) where

import           Protolude hiding (TypeError)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Cont
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common

data CommRes commPars i o e = CommRes commPars deriving Show
newtype SendMsg o = SendMsg o deriving (Show, Eq)
data GetNextMsg = GetNextMsg deriving (Show, Eq)
data CloseComm = CloseComm deriving (Show, Eq)
instance GetInstance CloseComm where getInstance = CloseComm
instance GetInstance GetNextMsg where getInstance = GetNextMsg
 
class CommProv commPars i o e m  where
  data StCommInitiated commPars i o e m 
  data StCommWaitForMsg commPars i o e m   
  data StCommMsgRcvd commPars i o e m 
  data StCommClosed commPars i o e m  
  data StCommFailed commPars i o e m   
  
  createComm :: (CommRes commPars i o e) -> m (StCommInitiated commPars i o e m) 
  sendMsgWaiting :: SendMsg o -> StCommWaitForMsg commPars i o e m   -> m (StCommWaitForMsg commPars i o e m) 
  sendMsgRcvd :: SendMsg o -> StCommMsgRcvd commPars i o e m   -> m (StCommMsgRcvd commPars i o e m) 
  getNextMsg :: GetNextMsg -> StCommMsgRcvd commPars i o e m   -> m (StCommWaitForMsg commPars i o e m)
  closeCommInit :: CloseComm ->  StCommInitiated commPars i o e m   -> m (StCommClosed commPars i o e m) 
  closeCommWaitng :: CloseComm ->  StCommWaitForMsg commPars i o e m   -> m (StCommClosed commPars i o e m) 
  closeCommRcvd :: CloseComm ->  StCommMsgRcvd commPars i o e m   -> m (StCommClosed commPars i o e m) 
  clearClosedComm :: StCommClosed commPars i o e m  -> m ()
  clearFailedComm :: StCommFailed commPars i o e m   -> m ()
  commInitTransition :: (StCommInitiated commPars i o e m)  -> ContT Bool m (Either (StCommFailed commPars i o e m)  (StCommWaitForMsg commPars i o e m))
  commWaitTransition :: (StCommWaitForMsg commPars i o e m)  -> ContT Bool m (Either (StCommClosed commPars i o e m)  (StCommMsgRcvd commPars i o e m))

  getRecvdMsg :: StCommMsgRcvd commPars i o e m  -> i
  getFailure :: StCommFailed commPars i o e m  -> e

type CommInitiated name commPars i o e m   = St (StCommInitiated commPars i o e m) name
type CommWaitForMsg name commPars i o e m   = St (StCommWaitForMsg commPars i o e m) name
type CommMsgRcvd name commPars i o e m  = St (StCommMsgRcvd commPars i o e m) name
type CommClosed name commPars i o e m  = St (StCommClosed commPars i o e m) name
type CommFailed name commPars i o e m   = St (StCommFailed commPars i o e m) name 

type instance Eval (SupportedRequests (CommInitiated name commPars i o e m)) = '[CloseComm]
type instance Eval (SupportedRequests (CommWaitForMsg name commPars i o e m)) = '[CloseComm, SendMsg o]
type instance Eval (SupportedRequests (CommMsgRcvd name commPars i o e m)) = '[GetNextMsg, CloseComm, SendMsg o]
type instance Eval (SupportedRequests (CommClosed name commPars i o e m)) = '[]
type instance Eval (SupportedRequests (CommFailed name commPars i o e m)) = '[]

type instance StateTitle (StCommInitiated commPars i o e m) = "CommInitiated"
type instance StateTitle (StCommWaitForMsg commPars i o e m) = "CommWaitForMsg"
type instance StateTitle (StCommMsgRcvd commPars i o e m) = "CommMsgRcvd"
type instance StateTitle (StCommClosed commPars i o e m) = "CommClosed"
type instance StateTitle (StCommFailed commPars i o e m) = "CommFailed"

instance (MonadIO m, CommProv commPars i o e m) => CreateRes m name (CommRes commPars i o e) (V '[CommInitiated name commPars i o e m ])  where
  createRes _nm pars = fmap (variantFromValue . St) (createComm pars)  

instance (MonadIO m, CommProv commPars i o e m) => MkRes m (CommRes commPars i o e) where
  type ResSt m (CommRes commPars i o e) = StCommInitiated commPars i o e m
  mkRes pars = createComm pars  
    
type instance StateTrans (CommClosed name commPars i o e m) = 'Static
type instance StateTrans (CommFailed name commPars i o e m) = 'Static
type instance StateTrans (CommInitiated name commPars i o e m) = 'Dynamic
type instance StateTrans (CommWaitForMsg name commPars i o e m) = 'Dynamic
type instance StateTrans (CommMsgRcvd name commPars i o e m) = 'Dynamic

type instance TermRequest (CommInitiated name commPars i o e m) = CloseComm
type instance TermRequest (CommWaitForMsg name commPars i o e m) = CloseComm
type instance TermRequest (CommMsgRcvd name commPars i o e m) = CloseComm

type family IsMessageReceivedFam a :: Bool where
  IsMessageReceivedFam (CommMsgRcvd name commPars i o e m) = 'True
  IsMessageReceivedFam _ = 'False
data IsMessageReceived :: Type -> Exp Bool 
type instance Eval (IsMessageReceived a) = IsMessageReceivedFam a

type family IsCommConnectedFam a :: Bool where
  IsCommConnectedFam (CommMsgRcvd name commPars i o e m) = 'True
  IsCommConnectedFam (CommWaitForMsg name commPars i o e m) = 'True
  IsCommConnectedFam _ = 'False
data IsCommConnected :: Type -> Exp Bool 
type instance Eval (IsCommConnected a) = IsCommConnectedFam a

type family IsCommAliveFam a :: Bool where
  IsCommAliveFam (CommMsgRcvd name commPars i o e m) = 'True
  IsCommAliveFam (CommWaitForMsg name commPars i o e m) = 'True
  IsCommAliveFam (CommInitiated name commPars i o e m) = 'True
  IsCommAliveFam _ = 'False
data IsCommAlive :: Type -> Exp Bool 
type instance Eval (IsCommAlive a) = IsCommAliveFam a 

instance (MonadIO m, CommProv commPars i o e m) => Request m CloseComm (CommInitiated name commPars i o e m) where
  type instance ReqResult CloseComm (CommInitiated name commPars i o e m) = '[CommClosed name commPars i o e m ]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommInit closeCommPars evData) 

instance (MonadIO m, CommProv commPars i o e m) => Request m CloseComm (CommWaitForMsg name commPars i o e m) where
  type instance ReqResult CloseComm (CommWaitForMsg name commPars i o e m) = '[CommClosed name commPars i o e m ]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommWaitng closeCommPars evData) 
  
instance (MonadIO m, CommProv commPars i o e m) => Request m CloseComm (CommMsgRcvd name commPars i o e m) where
  type instance ReqResult CloseComm (CommMsgRcvd name commPars i o e m) = '[CommClosed name commPars i o e m ]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommRcvd closeCommPars evData) 
      
instance (MonadIO m, CommProv commPars i o e m) => Request m (SendMsg o) (CommWaitForMsg name commPars i o e m) where
  type instance ReqResult (SendMsg o) (CommWaitForMsg name commPars i o e m) = '[CommWaitForMsg name commPars i o e m ]
  request sendMsgReq (St evData) = fmap (variantFromValue . St) (sendMsgWaiting sendMsgReq evData) 

instance (MonadIO m, CommProv commPars i o e m) => Request m (SendMsg o) (CommMsgRcvd name commPars i o e m) where
  type instance ReqResult (SendMsg o) (CommMsgRcvd name commPars i o e m) = '[CommMsgRcvd name commPars i o e m ]
  request sendMsgReq (St evData) = fmap (variantFromValue . St) (sendMsgRcvd sendMsgReq evData) 
  
instance (MonadIO m, CommProv commPars i o e m) => Request m GetNextMsg (CommMsgRcvd name commPars i o e m) where
  type instance ReqResult GetNextMsg (CommMsgRcvd name commPars i o e m) = '[CommWaitForMsg name commPars i o e m ]
  request getNextReq (St evData) = fmap (variantFromValue . St) (getNextMsg getNextReq evData) 
  
instance (MonadIO m, CommProv commPars i o e m) => TermState m (CommClosed name commPars i o e m) where
  terminate (St evData) = clearClosedComm evData
instance (MonadIO m, CommProv commPars i o e m) => TermState m (CommFailed name commPars i o e m) where
  terminate (St evData) = clearFailedComm evData

instance (MonadIO m, CommProv commPars i o e m) => Transition m (CommInitiated name commPars i o e m) where
  type instance NextStates (CommInitiated name commPars i o e m) = '[CommWaitForMsg name commPars i o e m , CommFailed name commPars i o e m ]
  next (St evData) cbFunc = runContT (commInitTransition evData) 
    (\case 
      Left failed -> cbFunc (toVariantAt @1 (St failed)) 
      Right wait -> cbFunc (toVariantAt @0 (St wait)))

instance (MonadIO m, CommProv commPars i o e m) => Transition m (CommWaitForMsg name commPars i o e m) where
  type instance NextStates (CommWaitForMsg name commPars i o e m) = '[CommMsgRcvd name commPars i o e m , CommClosed name commPars i o e m ]
  next (St evData) cbFunc = runContT (commWaitTransition evData)  
    (\case 
      Left clsd -> cbFunc (toVariantAt @1 (St clsd)) 
      Right rcvd -> cbFunc (toVariantAt @0 (St rcvd)))

instance (MonadIO m, CommProv commPars i o e m) => Transition m (CommMsgRcvd name commPars i o e m) where
  type instance NextStates (CommMsgRcvd name commPars i o e m) =
    TypeError ( 'Text "Use GetNextMessage before to read next incoming message") 
  next _ _ = undefined
                  
getIncomingMsg :: forall m name commPars i o e. CommProv commPars i o e m  => CommMsgRcvd name commPars i o e m  -> i
getIncomingMsg (St rcvdMsgState) = getRecvdMsg rcvdMsgState       
  
getCommFailure :: CommProv commPars i o e m  => CommFailed name commPars i o e m  -> e
getCommFailure (St commFailure) = getFailure commFailure
