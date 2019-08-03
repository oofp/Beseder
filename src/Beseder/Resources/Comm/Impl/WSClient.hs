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
{-# LANGUAGE RecordWildCards  #-}

module Beseder.Resources.Comm.Impl.WSClient 
  ( WSClient (..)
  )
  where

import           Protolude hiding (TypeError)
import           Control.Concurrent.STM.TVar
import           Haskus.Utils.Types
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.Comm.CommProv 
import           Beseder.Resources.Comm.CommProvImpl 
import qualified Network.WebSockets  as WS
import           Network.WebSockets.Connection
import qualified Data.ByteString.Lazy as LBStr
import           Prelude (String)
import           Data.Text
import           Network.Socket (withSocketsDo)
import           qualified GHC.Show (Show (..))

data WSClient = WSClient 
  { host :: Text
  , port :: Int
  , path :: Text
  , options :: ConnectionOptions
  , headers :: WS.Headers 
  , flEnablePing :: Bool
  } 

instance Show WSClient where
  show (WSClient {..}) = unpack ("ws://" <> host <> ":" <> show port <> path) 

data WsCommState 
  = WsCommOpenning
  | WsCommOpen
  | WsCommClosed

instance (MonadIO m) => CommProvImpl WSClient m LBStr.ByteString Text () where -- IOException where
  createProvImpl wsClient initCB msgCB = do
    liftIO $ putStrLn ("createProvImpl"::Text)
    wsCommState <- newTVarIO WsCommOpenning
    let completeHandling = 
          do
            curState <- readTVarIO wsCommState
            case curState of 
              WsCommOpenning -> initCB (Left ())
              WsCommOpen -> msgCB Nothing
              WsCommClosed -> return ()
    let exceptHandler :: IOException -> IO ()
        exceptHandler e = do
          putStrLn (("wsException:"::Text) <> show e)
          completeHandling  
    catch 
      (do
        withSocketsDo $ WS.runClientWith (unpack $ host wsClient) (port wsClient) (unpack $ path wsClient) (options wsClient) (headers wsClient) (appHandler wsCommState initCB msgCB)
        liftIO $ putStrLn ("createProvImpl:done"::Text)
        completeHandling
      ) exceptHandler


appHandler :: (MonadIO m) => TVar WsCommState -> (Either e (CommReqs m Text)->IO ()) -> (Maybe LBStr.ByteString -> IO ()) -> WS.ClientApp ()
appHandler wsCommState cb1 cb2 conn = 
  let commReqs = CommReqs (liftIO . WS.sendTextData conn) 
                          (liftIO $ sendClose conn (""::Text)  >>  (atomically $ writeTVar wsCommState WsCommClosed))
  in do
    liftIO $ putStrLn ("appHandler:entered"::Text)
    cb1 (Right commReqs)
    atomically $ writeTVar wsCommState WsCommOpen
    forever $ do
      msgRcvd <- WS.receiveData conn
      putStrLn (("Messages received:"::Text) <> show msgRcvd)
      cb2 (Just msgRcvd)


{-
data CommReqs m o  = CommReqs
  { sendReq :: o -> m ()
  , closeComm :: m ()
  }

  
class CommProvImpl commPars m i o e where
  createProvImpl :: commPars -> (Either e (CommReqs m o)->IO ()) -> (Maybe i -> IO ()) -> IO ()

-}

{-
data WSConnectingPars i o = WSConnectingPars i o
  { flPing :: Bool
  , connectingTask :: Async ()
  }

data WSConnectedPars i o = WSConnectedPars i o
  { conn :: Connection
  , connectedTask :: Async ()
  , pingTask :: Maybe Async ()
  }

  
type WSConnectingData (m :: * -> *) (name ::Symbol) i o = WithCB (WSConnectingPars i o) '[WSWaitForMsg m name i o, WSFailed m name] m
type WSWaitForMsgData (m :: * -> *) (name ::Symbol) i o = WithCB (WSConnectedPars i o) '[WSWaitMsgReceived m name i o, WSConnectionClosed m name] m

data WSWaitForMsgReceivedData (m :: * -> *) (name ::Symbol) i o = WithCB (WSConnectedPars i o, msgRcvd) '[] m
data WSConnectionClosedData (m :: * -> *) (name ::Symbol) = WSConnectionClosedData 
data WSFailedData (m :: * -> *) (name ::Symbol) = WSFailedData

type WSConnecting m name i o =    St (WSClnProv m name (WSConnectingData m name)) name
type WSWaitForMsg m name i o = St (WSClnProv m name (WSWaitForMsgData m name i o)) name
type WSWaitForMsgReceivedData m name i o = St (WSClnProv m name (WSWaitForMsgReceivedData m name i o)) name
type WSFailed m name = St (WSClnProv m name (WSFailedData m name)) name
type WSConnectionClosed m name = St (WSClnProv m name (WSConnectionClosedData m name)) name

instance Doc (WSClnProv m name WSConnecting m name i o) where
  doc _t = "WSConnecting"
instance Doc (WSClnProv m name (WSWaitForMsg m name i o)) where
  doc _t = "WSWaitForMsg"
instance Doc (WSClnProv m name (WSFailedData m name i o)) where
  doc _t = "WSFailed"
instance Doc (WSClnProv m name (WSConnectionClosedData m name)) where
  doc _t = "WSConnectionClosed"
    
data WSClient i o = WSClient 
  { host :: Text
  , port :: Int
  , path :: Text
  , options :: ConnectionOptions
  , header :: Headers 
  , flEnablePing :: Bool
  } deriving Show

instance (MonadIO m, WebSocketsData i, WebSocketsData o) => CreateRes m name (WSClient i o) (V '[WSConnecting m name i o]) where
  createRes  = createWSClientRes

createWSClientRes :: (MonadIO m, WebSocketsData i, WebSocketsData o) => Named name -> Timer -> m (V '[TimerNotArmed m name])
createWSClientRes _nm wsCln = do
  asyncTask <- liftIO $ async $ withSocketsDo $ WS.runClientWith (unpack (host wsCln)) (port wsCln) (unpack (path wsCln)) (options wsCln) (headers wsCln) conHandler
  fmap variantFromValue (newProvState TimerNotArmedData)

conHandler ::  WS.ClientApp ()
conHandler conn = undefined

type instance StateTrans (TimerNotArmed m name) = 'Static
type instance StateTrans (TimerTriggered m name) = 'Static
type instance StateTrans (TimerStopped m name) = 'Static

data TimerStateSum (m :: * -> *) name
  = TimerNotArmedState (TimerNotArmedData m name)
  | TimerArmedState (TimerArmedData m name) -- (WithCB (Async ()) (V '[WSClnProv m (TimerNotArmed m)]) m)
  | TimerTriggeredState (TimerTriggeredData m name)
  | TimerStoppedState (TimerStoppedData m name)

instance (Monad m) => Term (TimerNotArmedData m name) (TimerStateSum m name) where
  asSum = TimerNotArmedState
instance (Monad m) => Term (TimerArmedData m name) (TimerStateSum m name) where
  asSum = TimerArmedState
instance (Monad m) => Term (TimerTriggeredData m name) (TimerStateSum m name) where
  asSum = TimerTriggeredState
instance (Monad m) => Term (TimerStoppedData m name) (TimerStateSum m name) where
  asSum = TimerStoppedState
    
type WSClnProv (m :: * -> *) name st = ProvState (TimerStateSum m name) st

-- requests
newtype StartTimer = StartTimer Int deriving (Show, Eq)
data StopTimer = StopTimer deriving (Show, Eq)

--type instance ReqsOfStateList (TimerArmed m name) = '[StopTimer]
--type instance ReqsOfStateList (TimerNotArmed m name) = '[StartTimer]

instance (MonadIO m) => InitState m (V '[TimerNotArmed m name]) where
  initState = fmap variantFromValue (newProvState TimerNotArmedData)

instance (MonadIO m) => RequestHandler StopTimer (TimerArmedData m name) (TimerStateSum m name) m where
  type ResStateData StopTimer (TimerArmedData m name) = (TimerStoppedData m name)
  handleReq StopTimer stData1 cnt prvInfo = do
    liftIO $ cancel (dt stData1)
    return TimerStoppedData

instance (TaskPoster m) => RequestHandler StartTimer (TimerNotArmedData m name) (TimerStateSum m name) m where
  type ResStateData StartTimer (TimerNotArmedData m name) =  (TimerArmedData m name)
  handleReq (StartTimer timeoutSec) timerNotArmed cnt prvInfo = do
    liftIO $ putStrLn (("Start Timer requested..."::Text) <> (show cnt))
    taskPoster <- getTaskPoster
    asyncTimer <- liftIO $async $ do
      threadDelay (timeoutSec*1000000)
      taskPoster (handleTimeout (cnt+1) prvInfo)
    tvar <- liftIO $ newTVarIO Nothing
    return $ WithCB asyncTimer tvar

handleTimeout :: (MonadIO m) => Int -> ProvInfo (TimerStateSum m name) -> m Bool
handleTimeout stateCnt prvInfo = do
  --liftIO $ putStrLn ("Entered handleTimeout..."::Text)
  curState <- liftIO $ atomically $ readTVar (currentState prvInfo)
  curCnt <- liftIO $ atomically $ readTVar (currentCounter prvInfo)
  if curCnt == stateCnt
    then case curState of
      TimerArmedState timerArmedData -> do
        cbFncMaybe <- liftIO $ atomically $ readTVar (cbFunc timerArmedData)
        timerTriggered <- nextProvState prvInfo TimerTriggeredData
        case cbFncMaybe of
          Just cbFnc -> do
            -- liftIO $ putStrLn (("handleTimeout : fire timer "::Text) <> show curCnt)
            cbFnc (variantFromValue (St timerTriggered))
          Nothing -> do
            liftIO $ putStrLn ("handleTimeout no callback found "::Text)
            return True
      _ -> return True
    else do
      liftIO $ putStrLn (("handleTimeout counter mismatch "::Text) <> show stateCnt <> "/" <> show curCnt)
      return True

instance (MonadIO m) => TermState m (TimerNotArmed m name) where
  terminate _timerNotArmed = return ()
instance (MonadIO m) => TermState m (TimerTriggered m name) where
  terminate _timerTriggered = return ()
instance (MonadIO m) => TermState m (TimerStopped m name) where
  terminate _timerStopped = return ()
    
-}  