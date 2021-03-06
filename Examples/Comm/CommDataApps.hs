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
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  CommDataApps where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Control.Monad.Cont (ContT)                                               
import           Beseder.Base.ControlData
import           Beseder.Base.Control (STransApp (..))                                                
import           Beseder.Resources.Comm 
import           Beseder.Resources.State.DataRes
import           GHC.Exts (Any)    
import           Data.String

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

proxyDataApp :: forall m i1 i2 o1 o2 e1 e2 comm1 comm2. 
  ( _
  ) => CommRes comm1 i1 o1 e1 
      -> CommRes comm2 i2 o2 e2 
      -> (i1 -> o2) -> (i2 -> o1) -> (i1 -> Bool) 
      -> STransData m NoSplitter _ ()
proxyDataApp comRes1 comRes2 i1o2 i2o1 contPred = do
  block $ do
    newRes #com1 comRes1
    newRes #com2 comRes2
    newRes #flCont initAsTrue
  try @(("com1" :? IsCommAlive) :&& ("com2" :? IsCommAlive) :&& ("flCont" :? IsTrue)) $ do
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

mkSTransDataTypeAny "proxyDataApp" "ProxyApp"


-- :kind! Eval (ProxyApp NoSplitter '[()])
-- :kind!  ValidateSteps '[] ProxyApp NoSplitter '[()]  
-- :kind! StateDiagramSym ProxyApp '[()]
-- "[*] --> 1\n1 --> 2\n2 --> 3\n3 --> 4\n3 --> 5\n3 --> 6\n3 --> 7\n4 --> 8\n4 --> 9\n4 --> 10\n4 --> 11\n5 --> 8\n5 --> 12\n5 --> 13\n8 --> 14\n8 --> 15\n8 --> 16\n6 --> [*]\n7 --> [*]\n9 --> [*]\n10 --> [*]\n12 --> [*]\n13 --> [*]\n14 --> [*]\n15 --> [*]\n11 --> [*]\n16 --> [*]1 : com1 = CommInitiated\n2 : com1 = CommInitiated\n2 : com2 = CommInitiated\n3 : com1 = CommInitiated\n3 : com2 = CommInitiated\n3 : flCont = true\n4 : com1 = CommWaitForMsg\n4 : com2 = CommInitiated\n4 : flCont = true\n5 : com1 = CommInitiated\n5 : com2 = CommWaitForMsg\n5 : flCont = true\n6 : com1 = CommFailed\n6 : com2 = CommInitiated\n6 : flCont = true\n7 : com1 = CommInitiated\n7 : com2 = CommFailed\n7 : flCont = true\n8 : com1 = CommWaitForMsg\n8 : com2 = CommWaitForMsg\n8 : flCont = true\n9 : com1 = CommClosed\n9 : com2 = CommInitiated\n9 : flCont = true\n10 : com1 = CommWaitForMsg\n10 : com2 = CommFailed\n10 : flCont = true\n11 : com1 = CommMsgRcvd\n11 : com2 = CommInitiated\n11 : flCont = false\n12 : com1 = CommFailed\n12 : com2 = CommWaitForMsg\n12 : flCont = true\n13 : com1 = CommInitiated\n13 : com2 = CommClosed\n13 : flCont = true\n14 : com1 = CommClosed\n14 : com2 = CommWaitForMsg\n14 : flCont = true\n15 : com1 = CommWaitForMsg\n15 : com2 = CommClosed\n15 : flCont = true\n16 : com1 = CommMsgRcvd\n16 : com2 = CommWaitForMsg\n16 : flCont = false\n"
-- "[*] --> 1\n1 --> 2\n2 --> 3\n3 --> 4\n3 --> 5\n3 --> 6\n3 --> 7\n4 --> 8\n4 --> 9\n4 --> 10\n4 --> 11\n5 --> 9\n5 --> 12\n5 --> 13\n5 --> 14\n9 --> 15\n9 --> 16\n9 --> 17\n9 --> 18\n8 --> 19\n15 --> 20\n15 --> 9\n8 --> 4\n16 --> 9\n12 --> 5\n6 --> [*]\n7 --> [*]\n10 --> [*]\n11 --> [*]\n13 --> [*]\n14 --> [*]\n17 --> [*]\n18 --> [*]\n19 --> [*]\n20 --> [*]1 : com1 = CommInitiated\n2 : com1 = CommInitiated\n2 : com2 = CommInitiated\n3 : com1 = CommInitiated\n3 : com2 = CommInitiated\n3 : flCont = true\n4 : com1 = CommWaitForMsg\n4 : com2 = CommInitiated\n4 : flCont = true\n5 : com1 = CommInitiated\n5 : com2 = CommWaitForMsg\n5 : flCont = true\n6 : com1 = CommFailed\n6 : com2 = CommInitiated\n6 : flCont = true\n7 : com1 = CommInitiated\n7 : com2 = CommFailed\n7 : flCont = true\n8 : com1 = CommMsgRcvd\n8 : com2 = CommInitiated\n8 : flCont = true\n9 : com1 = CommWaitForMsg\n9 : com2 = CommWaitForMsg\n9 : flCont = true\n10 : com1 = CommClosed\n10 : com2 = CommInitiated\n10 : flCont = true\n11 : com1 = CommWaitForMsg\n11 : com2 = CommFailed\n11 : flCont = true\n12 : com1 = CommInitiated\n12 : com2 = CommMsgRcvd\n12 : flCont = true\n13 : com1 = CommFailed\n13 : com2 = CommWaitForMsg\n13 : flCont = true\n14 : com1 = CommInitiated\n14 : com2 = CommClosed\n14 : flCont = true\n15 : com1 = CommMsgRcvd\n15 : com2 = CommWaitForMsg\n15 : flCont = true\n16 : com1 = CommWaitForMsg\n16 : com2 = CommMsgRcvd\n16 : flCont = true\n17 : com1 = CommClosed\n17 : com2 = CommWaitForMsg\n17 : flCont = true\n18 : com1 = CommWaitForMsg\n18 : com2 = CommClosed\n18 : flCont = true\n19 : com1 = CommMsgRcvd\n19 : com2 = CommInitiated\n19 : flCont = false\n20 : com1 = CommMsgRcvd\n20 : com2 = CommWaitForMsg\n20 : flCont = false\n"
-- "[*] --> 1\n1 --> 2\n1 --> 3\n1 --> 4\n1 --> 5\n2 --> 6\n2 --> 7\n2 --> 8\n2 --> 9\n3 --> 7\n3 --> 10\n3 --> 11\n3 --> 12\n7 --> 13\n7 --> 14\n7 --> 15\n7 --> 16\n6 --> 17\n13 --> 18\n13 --> 7\n6 --> 2\n14 --> 7\n10 --> 3\n4 --> [*]\n5 --> [*]\n8 --> [*]\n9 --> [*]\n11 --> [*]\n12 --> [*]\n15 --> [*]\n16 --> [*]\n17 --> [*]\n18 --> [*]\n1 : com1 = CommInitiated\n1 : com2 = CommInitiated\n1 : flCont = true\n2 : com1 = CommWaitForMsg\n2 : com2 = CommInitiated\n2 : flCont = true\n3 : com1 = CommInitiated\n3 : com2 = CommWaitForMsg\n3 : flCont = true\n4 : com1 = CommFailed\n4 : com2 = CommInitiated\n4 : flCont = true\n5 : com1 = CommInitiated\n5 : com2 = CommFailed\n5 : flCont = true\n6 : com1 = CommMsgRcvd\n6 : com2 = CommInitiated\n6 : flCont = true\n7 : com1 = CommWaitForMsg\n7 : com2 = CommWaitForMsg\n7 : flCont = true\n8 : com1 = CommClosed\n8 : com2 = CommInitiated\n8 : flCont = true\n9 : com1 = CommWaitForMsg\n9 : com2 = CommFailed\n9 : flCont = true\n10 : com1 = CommInitiated\n10 : com2 = CommMsgRcvd\n10 : flCont = true\n11 : com1 = CommFailed\n11 : com2 = CommWaitForMsg\n11 : flCont = true\n12 : com1 = CommInitiated\n12 : com2 = CommClosed\n12 : flCont = true\n13 : com1 = CommMsgRcvd\n13 : com2 = CommWaitForMsg\n13 : flCont = true\n14 : com1 = CommWaitForMsg\n14 : com2 = CommMsgRcvd\n14 : flCont = true\n15 : com1 = CommClosed\n15 : com2 = CommWaitForMsg\n15 : flCont = true\n16 : com1 = CommWaitForMsg\n16 : com2 = CommClosed\n16 : flCont = true\n17 : com1 = CommMsgRcvd\n17 : com2 = CommInitiated\n17 : flCont = false\n18 : com1 = CommMsgRcvd\n18 : com2 = CommWaitForMsg\n18 : flCont = false\n"

{-
proxyTrans :: forall m i1 i2 o1 o2 e1 e2 comm1 comm2. 
  ( _
  ) => CommRes comm1 i1 o1 e1 
      -> CommRes comm2 i2 o2 e2 
      -> (i1 -> o2) -> (i2 -> o1) -> (i1 -> Bool) 
      -> STrans (ContT Bool) m NoSplitter '[()] _ _ _ () 
proxyTrans comRes1 comRes2 i1o2 i2o1 contPred = interpret (proxyDataApp comRes1 comRes2 i1o2 i2o1 contPred)
-}

proxyApp :: forall m i1 i2 o1 o2 e1 e2 comm1 comm2. 
  ( _
  ) => CommRes comm1 i1 o1 e1 
      -> CommRes comm2 i2 o2 e2 
      -> (i1 -> o2) -> (i2 -> o1) -> (i1 -> Bool) 
      -> STransApp (ContT Bool) m NoSplitter '[()] _ _ () 
proxyApp comRes1 comRes2 i1o2 i2o1 contPred = MkApp $ interpret (proxyDataApp comRes1 comRes2 i1o2 i2o1 contPred)
