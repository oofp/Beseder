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

module Beseder.Resources.Comm.Impl.Console 
  ( ConsoleRes
  , consoleRes
  , Console (..)
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
import           Beseder.Resources.Comm.CommProvImpl 

import           Prelude (String)
import           Data.Text

data Console = Console deriving Show 
type ConsoleRes = CommRes Console Text Text ()

consoleRes :: ConsoleRes
consoleRes = CommRes Console 

instance (MonadIO m) => CommProvImpl Console m Text Text () where 
  createProvImpl _console initCB msgCB = 
    let commReqs = CommReqs (liftIO . putStrLn) (return ())
        exceptHandler :: IOException -> IO ()
        exceptHandler _e = msgCB Nothing
    in do
      liftIO $ putStrLn ("createProvImpl"::Text)
      initCB (Right commReqs)
      catch 
        (forever $ do 
          inp <- getLine
          msgCB $ Just inp)
        exceptHandler

