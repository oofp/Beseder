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
{-# LANGUAGE LambdaCase   #-}

module Beseder.Resources.Comm.CommProvImpl  
  ( CommProvImpl (..)
  , CommReqs (..)
  )
  where

import           Protolude hiding (TypeError)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Cont
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.Comm.CommProv 
    
data CommReqs m o  = CommReqs
  { sendReq :: o -> m ()
  , closeComm :: m ()
  }
  
class CommProvImpl commPars m i o e where
  createProvImpl :: commPars -> (Either e (CommReqs m o)->IO ()) -> (Maybe i -> IO ()) -> IO ()


data CommState commPars m i o e = CommState 
  { asyncTask :: Maybe (Async ())
  , commReqs :: Maybe (CommReqs m o)
  , cb1 :: Maybe (Either (StCommFailed m i o e commPars) (StCommWaitForMsg m i o e commPars)-> m Bool)
  , cb2 :: Maybe (Either (StCommClosed m i o e commPars) (StCommMsgRcvd m i o e commPars)-> m Bool)
  }

setAsyncTask :: TaskPoster m => Async () -> TVar (CommState commPars m i o e) -> m ()
setAsyncTask asncTask tvar = liftIO $ atomically $ modifyTVar tvar (\comSt -> comSt {asyncTask = Just asncTask})

sendMessage :: TaskPoster m => TVar (CommState commPars m i o e) -> o -> m ()
sendMessage comStTVar o = do  
  commReqsMaybe <- liftIO $ atomically $ do
    comSt <- readTVar comStTVar
    writeTVar comStTVar (comSt {cb2=Nothing}) -- should be set again by next transition
    return $ (commReqs comSt)
  case commReqsMaybe of 
    Nothing -> unexpectedState "commReqs is Nothing at sendMessage"
    Just commRqs -> sendReq commRqs o
  
cancelComm :: TaskPoster m => TVar (CommState commPars m i o e) -> m (StCommClosed m i o e commPars)
cancelComm comStTVar = do
  commSt <- liftIO $ atomically $ do
    comSt <- readTVar comStTVar
    writeTVar comStTVar (comSt {cb1=Nothing, cb2=Nothing}) -- should be set again by next transition
    return comSt
  case (commReqs commSt) of 
    Nothing -> unexpectedState "commReqs is Nothing at closeComm"
    Just commRqs -> closeComm commRqs
  case (asyncTask commSt) of 
    Nothing -> unexpectedState "asyncTask is Nothing at closeComm"
    Just asncTask -> void $ liftIO $ async $ cancel asncTask >> putStrLn ("comm cancelled"::Protolude.Text)
  return StCommClosed  

instance (CommProvImpl commPars m i o e, TaskPoster m) => CommProv m i o e commPars where  
  data StCommInitiated m i o e commPars =  StCommInitiated (TVar (CommState commPars m i o e))
  data StCommWaitForMsg m i o e commPars = StCommWaitForMsg (TVar (CommState commPars m i o e))
  data StCommMsgRcvd m i o e commPars = StCommMsgRcvd (TVar (CommState commPars m i o e)) i
  data StCommClosed m i o e commPars = StCommClosed 
  data StCommFailed m i o e commPars = StCommFailed e 
  
  createComm (CommRes commPars) = do
    liftIO $ putStrLn ("creatComn entered:"::Text)
    tvarState <- liftIO $ newTVarIO (CommState Nothing Nothing Nothing Nothing)
    taskPoster <- getTaskPoster
    asncTask <- liftIO $ async $ createProvImpl commPars (taskPoster . handleInitiatedState tvarState) (taskPoster . handleConnectedState tvarState) 
    setAsyncTask asncTask tvarState
    return $ StCommInitiated tvarState

  sendMsgWaiting (SendMsg o) st@(StCommWaitForMsg commState) = do
    sendMessage commState o
    return st

  sendMsgRcvd (SendMsg o) st@(StCommMsgRcvd commState _i) = do
    sendMessage commState o
    return st

  getNextMsg GetNextMsg (StCommMsgRcvd commState _i) = return (StCommWaitForMsg commState)

  closeCommInit CloseComm (StCommInitiated commState) = cancelComm commState 
  closeCommWaitng CloseComm (StCommWaitForMsg commState) = cancelComm commState
  closeCommRcvd CloseComm (StCommMsgRcvd commState _i) = cancelComm commState 
  clearClosedComm StCommClosed = return ()
  clearFailedComm (StCommFailed _) = return ()

  commInitTransition (StCommInitiated  commState) = 
    let contFunc cbFunc = liftIO $ atomically $ modifyTVar commState (\comSt -> comSt {cb1=Just cbFunc}) >> return True
    in ContT contFunc  

  commWaitTransition (StCommWaitForMsg commState) =
    let contFunc cbFunc = liftIO $ atomically $ modifyTVar commState (\comSt -> comSt {cb2=Just cbFunc}) >> return True
    in ContT contFunc

  getRecvdMsg (StCommMsgRcvd commState i) = i
  getFailure (StCommFailed e) = e

handleInitiatedState :: (TaskPoster m) => TVar (CommState commPars m i o e) -> Either e (CommReqs m o) -> m (Bool)
handleInitiatedState  comStTVar errOrComm = do
  cb1Maybe <- liftIO $ atomically $ do
    comSt <- readTVar comStTVar
    case errOrComm of 
      Left e -> writeTVar comStTVar (comSt {cb1=Nothing})
      Right commRqs -> writeTVar comStTVar (comSt {cb1=Nothing, commReqs=Just commRqs})
    return $ (cb1 comSt)
  case cb1Maybe of 
    Nothing -> unexpectedState ("handleInitiatedState, cb1 is Nothing"::Text) >> return True
    Just cb1Func -> 
      case errOrComm of 
        Left e -> cb1Func (Left $ StCommFailed e)
        Right commRqs -> cb1Func (Right $ StCommWaitForMsg comStTVar)
 

handleConnectedState :: (TaskPoster m) => TVar (CommState commPars m i o e) -> Maybe i -> m (Bool)
handleConnectedState  comStTVar msgMaybe = do
  cb2Maybe <- liftIO $ atomically $ do
    comSt <- readTVar comStTVar
    writeTVar comStTVar (comSt {cb2=Nothing})
    return $ (cb2 comSt)
  case cb2Maybe of 
    Nothing -> unexpectedState ("handleConnectedState, cb2 is Nothing"::Text) >> return True
    Just cb2Func -> 
      case msgMaybe of 
        Nothing -> cb2Func (Left $ StCommClosed)
        Just msg -> cb2Func (Right $ StCommMsgRcvd comStTVar msg)

unexpectedState :: (MonadIO m) => Text -> m ()
unexpectedState = putStrLn -- must not happen
