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
import           Beseder.Resources.Comm.CommProvImpl 
import qualified Network.WebSockets  as WS
import           Network.WebSockets.Connection
import qualified Data.ByteString.Lazy as LBStr
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

