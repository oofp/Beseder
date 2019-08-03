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

class CommProv m i o e commPars where
  data StCommInitiated m i o e commPars
  data StCommWaitForMsg m i o e commPars  
  data StCommMsgRcvd m i o e commPars
  data StCommClosed m i o e commPars 
  data StCommFailed m i o e commPars  
  
  createComm :: (CommRes commPars i o e) -> m (StCommInitiated m i o e commPars) 
  sendMsgWaiting :: SendMsg o -> StCommWaitForMsg m i o e commPars  -> m (StCommWaitForMsg m i o e commPars) 
  sendMsgRcvd :: SendMsg o -> StCommMsgRcvd m i o e commPars  -> m (StCommMsgRcvd m i o e commPars) 
  getNextMsg :: GetNextMsg -> StCommMsgRcvd m i o e commPars  -> m (StCommWaitForMsg m i o e commPars)
  closeCommInit :: CloseComm ->  StCommInitiated m i o e commPars  -> m (StCommClosed m i o e commPars) 
  closeCommWaitng :: CloseComm ->  StCommWaitForMsg m i o e commPars  -> m (StCommClosed m i o e commPars) 
  closeCommRcvd :: CloseComm ->  StCommMsgRcvd m i o e commPars  -> m (StCommClosed m i o e commPars) 
  clearClosedComm :: StCommClosed m i o e commPars -> m ()
  clearFailedComm :: StCommFailed m i o e commPars  -> m ()
  commInitTransition :: (StCommInitiated m i o e commPars)  -> ContT Bool m (Either (StCommFailed m i o e commPars)  (StCommWaitForMsg m i o e commPars))
  commWaitTransition :: (StCommWaitForMsg m i o e commPars)  -> ContT Bool m (Either (StCommClosed m i o e commPars)  (StCommMsgRcvd m i o e commPars))

  getRecvdMsg :: StCommMsgRcvd m i o e commPars -> i
  getFailure :: StCommFailed m i o e commPars -> e

type CommInitiated name m i o e commPars  = St (StCommInitiated m i o e commPars) name
type CommWaitForMsg name m i o e commPars  = St (StCommWaitForMsg m i o e commPars) name
type CommMsgRcvd name m i o e commPars = St (StCommMsgRcvd m i o e commPars) name
type CommClosed name m i o e commPars = St (StCommClosed m i o e commPars) name
type CommFailed name m i o e commPars  = St (StCommFailed m i o e commPars) name 

instance (MonadIO m, CommProv m i o e commPars) => CreateRes m name (CommRes commPars i o e) (V '[CommInitiated name m i o e commPars])  where
  createRes _nm pars = fmap (variantFromValue . St) (createComm pars)  

type instance StateTrans (CommClosed name m i o e commPars) = 'Static
type instance StateTrans (CommFailed name m i o e commPars) = 'Static
type instance StateTrans (CommInitiated name m i o e commPars) = 'Dynamic
type instance StateTrans (CommWaitForMsg name m i o e commPars) = 'Dynamic
type instance StateTrans (CommMsgRcvd name m i o e commPars) = 'Dynamic

type instance TermRequest (CommInitiated name m i o e commPars) = CloseComm
type instance TermRequest (CommWaitForMsg name m i o e commPars) = CloseComm
type instance TermRequest (CommMsgRcvd name m i o e commPars) = CloseComm

type family IsMessageReceivedFam a :: Bool where
  IsMessageReceivedFam (CommMsgRcvd name m i o e commPars) = 'True
  IsMessageReceivedFam _ = 'False
data IsMessageReceived :: Type -> Exp Bool 
type instance Eval (IsMessageReceived a) = IsMessageReceivedFam a

type family IsCommConnectedFam a :: Bool where
  IsCommConnectedFam (CommMsgRcvd name m i o e commPars) = 'True
  IsCommConnectedFam (CommWaitForMsg name m i o e commPars) = 'True
  IsCommConnectedFam _ = 'False
data IsCommConnected :: Type -> Exp Bool 
type instance Eval (IsCommConnected a) = IsCommConnectedFam a

type family IsCommAliveFam a :: Bool where
  IsCommAliveFam (CommMsgRcvd name m i o e commPars) = 'True
  IsCommAliveFam (CommWaitForMsg name m i o e commPars) = 'True
  IsCommAliveFam (CommInitiated name m i o e commPars) = 'True
  IsCommAliveFam _ = 'False
data IsCommAlive :: Type -> Exp Bool 
type instance Eval (IsCommAlive a) = IsCommAliveFam a 

instance (MonadIO m, CommProv m i o e commPars) => Request m CloseComm (CommInitiated name m i o e commPars) where
  type instance ReqResult CloseComm (CommInitiated name m i o e commPars) = '[CommClosed name m i o e commPars]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommInit closeCommPars evData) 

instance (MonadIO m, CommProv m i o e commPars) => Request m CloseComm (CommWaitForMsg name m i o e commPars) where
  type instance ReqResult CloseComm (CommWaitForMsg name m i o e commPars) = '[CommClosed name m i o e commPars]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommWaitng closeCommPars evData) 
  
instance (MonadIO m, CommProv m i o e commPars) => Request m CloseComm (CommMsgRcvd name m i o e commPars) where
  type instance ReqResult CloseComm (CommMsgRcvd name m i o e commPars) = '[CommClosed name m i o e commPars]
  request closeCommPars (St evData) = fmap (variantFromValue . St) (closeCommRcvd closeCommPars evData) 
      
instance (MonadIO m, CommProv m i o e commPars) => Request m (SendMsg o) (CommWaitForMsg name m i o e commPars) where
  type instance ReqResult (SendMsg o) (CommWaitForMsg name m i o e commPars) = '[CommWaitForMsg name m i o e commPars]
  request sendMsgReq (St evData) = fmap (variantFromValue . St) (sendMsgWaiting sendMsgReq evData) 

instance (MonadIO m, CommProv m i o e commPars) => Request m (SendMsg o) (CommMsgRcvd name m i o e commPars) where
  type instance ReqResult (SendMsg o) (CommMsgRcvd name m i o e commPars) = '[CommMsgRcvd name m i o e commPars]
  request sendMsgReq (St evData) = fmap (variantFromValue . St) (sendMsgRcvd sendMsgReq evData) 
  
instance (MonadIO m, CommProv m i o e commPars) => Request m GetNextMsg (CommMsgRcvd name m i o e commPars) where
  type instance ReqResult GetNextMsg (CommMsgRcvd name m i o e commPars) = '[CommWaitForMsg name m i o e commPars]
  request getNextReq (St evData) = fmap (variantFromValue . St) (getNextMsg getNextReq evData) 
  
instance (MonadIO m, CommProv m i o e commPars) => TermState m (CommClosed name m i o e commPars) where
  terminate (St evData) = clearClosedComm evData
instance (MonadIO m, CommProv m i o e commPars) => TermState m (CommFailed name m i o e commPars) where
  terminate (St evData) = clearFailedComm evData

instance (MonadIO m, CommProv m i o e commPars) => Transition m (CommInitiated name m i o e commPars) where
  type instance NextStates (CommInitiated name m i o e commPars) = '[CommWaitForMsg name m i o e commPars, CommFailed name m i o e commPars]
  next (St evData) cbFunc = runContT (commInitTransition evData) 
    (\case 
      Left failed -> cbFunc (toVariantAt @1 (St failed)) 
      Right wait -> cbFunc (toVariantAt @0 (St wait)))

instance (MonadIO m, CommProv m i o e commPars) => Transition m (CommWaitForMsg name m i o e commPars) where
  type instance NextStates (CommWaitForMsg name m i o e commPars) = '[CommMsgRcvd name m i o e commPars, CommClosed name m i o e commPars]
  next (St evData) cbFunc = runContT (commWaitTransition evData)  
    (\case 
      Left clsd -> cbFunc (toVariantAt @1 (St clsd)) 
      Right rcvd -> cbFunc (toVariantAt @0 (St rcvd)))

instance (MonadIO m, CommProv m i o e commPars) => Transition m (CommMsgRcvd name m i o e commPars) where
  type instance NextStates (CommMsgRcvd name m i o e commPars) =
    TypeError ( 'Text "Use GetNextMessage before to read next incoming message") 
  next _ _ = undefined
                  
getIncomingMsg :: CommProv m i o e commPars => CommMsgRcvd name m i o e commPars -> i
getIncomingMsg (St rcvdMsgState) = getRecvdMsg rcvdMsgState       
  
getCommFailure :: CommProv m i o e commPars => CommFailed name m i o e commPars -> e
getCommFailure (St commFailure) = getFailure commFailure
