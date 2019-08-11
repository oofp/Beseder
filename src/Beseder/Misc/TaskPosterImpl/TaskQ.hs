{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Beseder.Misc.TaskPosterImpl.TaskQ where

import Protolude    
import Beseder.Base.Base
import Beseder.Base.Common
import Beseder.Base.Control hiding ((>>), return)
import Beseder.Base.Internal.STransFunc
import Beseder.Misc.TaskPosterImpl.CallbackQueue
import Control.Concurrent.STM.TChan
import Control.Monad.Cont
import Control.Monad.Identity

type TaskQ  = ReaderT QueueChannel IO

newtype QueueChannel = QueueChannel {chan :: TChan (TaskQ Bool)}
instance HasCallbackChan TaskQ QueueChannel where
  getChan (QueueChannel ch) = ch

newQueue :: IO QueueChannel
newQueue = QueueChannel <$> newTChanIO

submitFlow :: TaskQ () -> TaskQ ()  
submitFlow task =  task >> runQueue

--runMMonadQueue :: MMonad () -> MMonad ()  
--runMMonadQueue mm =  mm >> runQueue

runTaskQ :: TaskQ () -> IO ()
runTaskQ rd = do
  que <- newQueue
  runReaderT rd que

  
runAsyncFlow :: ContT Bool TaskQ () -> IO ()
runAsyncFlow qm = runTaskQ $ submitFlow $ void $ runContT qm (const $ return False)

runSyncFlow :: IdentityT TaskQ () -> IO ()
runSyncFlow qm = runTaskQ $ void $ runIdentityT qm 

runAsyncTrans :: (ExecutableFunc sfunc) => ExcecutableTrans (ContT Bool) TaskQ sfunc -> IO ()
runAsyncTrans = runAsyncFlow . execTrans  

runSyncTrans :: (ExecutableFunc sfunc) => ExcecutableTrans IdentityT TaskQ sfunc -> IO ()
runSyncTrans = runSyncFlow . execTrans  

runAsyncApp ::  ExcecutableApp (ContT Bool) TaskQ -> IO ()
runAsyncApp = runAsyncFlow . execApp 

reifyAsyncTrans :: forall d f xs a. ToTrans f d (ContT Bool) TaskQ NoSplitter xs a => Proxy f -> Proxy d -> STrans (ContT Bool) TaskQ NoSplitter xs (Eval (f NoSplitter xs)) f a
reifyAsyncTrans px_f px_d = reifyTrans px_f px_d