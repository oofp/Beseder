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

module Beseder.Resources.Comm.Impl.STMComm 
  ( STMRes
  , stmRes
  , STMComm (..)
  )
  where

import           Protolude hiding (TypeError)
import           Beseder.Resources.Comm.CommProv 
import           Beseder.Resources.Comm.CommProvImpl 

data STMComm a 
  = STMComm 
  { stmRecv :: STM (Maybe a)
  , stmSend :: a -> STM ()
  , stmAckMsg :: a -> a
  , stmCloseMsg :: Maybe a
  }

type STMRes a = CommRes (STMComm a) a a ()

stmRes :: STMComm a -> STMRes a
stmRes = CommRes  

instance (MonadIO m) => CommProvImpl (STMComm a) m a a () where 
  createProvImpl stmComm initCB msgCB = 
    let 
      goRecv = do
        inp <- atomically $ 
          do maybeA <- stmRecv stmComm
             case maybeA of
              (Just a) -> do
                (stmSend stmComm) ((stmAckMsg stmComm) a)
                return a
              Nothing -> retry 
        msgCB $ Just inp
        goRecv 
      commReqs = 
          CommReqs
            (liftIO . atomically . stmSend stmComm) 
            (forM_ (stmCloseMsg stmComm) (liftIO . atomically . stmSend stmComm))
      exceptHandler :: IOException -> IO ()
      exceptHandler _e = msgCB Nothing
    in do
      liftIO $ putStrLn ("createProvImpl (STMComm)"::Text)
      initCB (Right commReqs)
      catch 
        goRecv
        exceptHandler

