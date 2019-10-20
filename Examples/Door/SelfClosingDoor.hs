{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  SelfClosingDoor 
  ( newSelfClosingDoor
  , openDoor
  , closeDoor
  , lockDoor
  , unlockDoor
  , DoorCfg (..)
  , IsDoorLocked
  , IsDoorOpen
  , IsDoorClosed
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, First)
import qualified Protolude as P                                               
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Data.String

-- import           Beseder.Misc.Misc 
--- !!! Loading this module breaks compilation
{-
    * Could not deduce (Beseder.Misc.TaskPosterImpl.CallbackQueue.HasCallbackChan
                          m r1)
        arising from a use of `hfunc'
      from the context: TaskPoster m
-}
import           Beseder.Resources.State.DataRes
import           Beseder.Resources.Timer
import           Beseder.Resources.Composite
import           PureDoor 
import           Control.Monad.Identity (IdentityT)

data DoorCfg = DoorCfg 
  { openTimeout :: Int
  , closedTimeout :: Int
  } 

doorInit :: TaskPoster m => DoorCfg -> STransApp IdentityT m NoSplitter '[()] _ '[] ()
doorInit doorCfg = MkApp $ do
  newRes #cfg (InitData doorCfg)
  newRes #door doorRes
  newRes #closedTimer TimerRes
  invoke  #closedTimer (StartTimer (closedTimeout doorCfg))

selfClosingDoor :: forall (m :: * -> *). TaskPoster m => DoorCfg ->  m (CResPar m (DoorHnd m) _)
selfClosingDoor cfg = P.return $ compRes @(DoorHnd m) (doorInit cfg) 

newSelfClosingDoor :: forall (m :: * -> *) q sp tt. (_) => Named tt -> DoorCfg -> STrans q m sp _ _ _ _ ()
newSelfClosingDoor named cfg = do
  res <- op $ selfClosingDoor cfg
  newRes named res

doCloseOpenedDoor :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doCloseOpenedDoor = do
  clear #openTimer
  doCloseDoor

startClosedDoorTimer :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
startClosedDoorTimer = do
  newRes #closedTimer TimerRes
  timeout <- gets #cfg (closedTimeout . getData)
  invoke  #closedTimer (StartTimer timeout)

doCloseDoor :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doCloseDoor = do
  invoke #door CloseDoor
  startClosedDoorTimer

doUnlockDoor :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doUnlockDoor = do
    invoke #door UnlockDoor
    startClosedDoorTimer

doOpenDoor :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doOpenDoor = do
    invoke #door OpenDoor
    newRes #openTimer TimerRes
    timeout <- gets #cfg (openTimeout . getData)
    invoke  #openTimer (StartTimer timeout)

doLockDoor :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doLockDoor = do
    clear #closedTimer
    invoke #door LockDoor

--doorHandler :: (TaskPoster m, _) => STrans IdentityT m NoSplitter xs _ _ _ ()
doorHandler :: (_) => STrans IdentityT m NoSplitter xs _ _ _ ()
doorHandler = do 
  on @("closedTimer" :? IsTimerTriggered) $ doLockDoor 
  on @("openTimer" :? IsTimerTriggered) $ doCloseOpenedDoor

type DoorFunc m = 
  (ComposeFunc
    (CaptureFunc
      ("closedTimer" :? IsTimerTriggered)
      (ComposeFunc
          (ClearAllFunc "closedTimer")
          (InvokeAllFunc LockDoor "door")))
    (CaptureFunc
      ("openTimer" :? IsTimerTriggered)
      (ComposeFunc
          (ClearAllFunc "openTimer")
          (ComposeFunc
            (InvokeAllFunc CloseDoor "door")
            (ComposeFunc
                (NewResFunc TimerRes "closedTimer" m)
                (BindFunc
                  IDFunc
                  (InvokeAllFunc StartTimer "closedTimer")))))))

hfunc =extractHandler doorHandler  

data DoorHnd :: (* -> *) -> * -> Exp [*]
type instance Eval (DoorHnd m res) = First (Eval (DoorFunc m NoSplitter '[res]))

type Cfg = StD DoorCfg "cfg"

type SelfDoorOpenTriggered m = (Cfg, (DoorOpen "door", TimerTriggered m "openTimer")) 
type SelfDoorClosedTriggered m = (Cfg, (DoorClosed "door", TimerTriggered m "closedTimer")) 
-- :kind! (Eval (DoorHnd IO (SelfDoorOpenTriggered  IO)))
-- :kind! (Eval (DoorHnd IO (SelfDoorClosedTriggered  IO)))

instance (TaskPoster m) => StateHandlerProv m (DoorHnd m) (SelfDoorOpenTriggered m) where getHandler = const hfunc 
instance (TaskPoster m) => StateHandlerProv m (DoorHnd m) (SelfDoorClosedTriggered m) where getHandler = const hfunc  

type SelfDoorOpen m = (Cfg, (DoorOpen "door", TimerArmed m "openTimer")) 
type SelfDoorClosed m = (Cfg, (DoorClosed "door", TimerArmed m "closedTimer")) 
type SelfDoorLocked m = (Cfg, DoorLocked "door") 

closeDoor :: (_) => m (CompReq m (SelfDoorOpen m) _)
closeDoor = P.return $ compReq $ MkApp $ do 
  liftIO $ putStrLn ("Close door"::Text)
  invoke #openTimer StopTimer 
  doCloseDoor

unlockDoor :: (_) => m (CompReq m (SelfDoorLocked m) _)
unlockDoor = P.return $ compReq $ MkApp $ do
  liftIO $ putStrLn ("Unlock door"::Text)
  doUnlockDoor 

invokeUnlockDoor ::  (_) => Named name -> STrans q m sp _ _ _ _ ()
invokeUnlockDoor named =  do
  req <- op unlockDoor
  invoke named req

lockDoor :: (_) => m (CompReq m (SelfDoorClosed m) _)
lockDoor = P.return $ compReq $ MkApp $ do 
  liftIO $ putStrLn ("Lock door"::Text)
  doLockDoor 

openDoor :: (_) => m (CompReq m (SelfDoorClosed m) _)
openDoor = P.return $ compReq $ MkApp $ do
  liftIO $ putStrLn ("Open door"::Text)
  invoke #closedTimer StopTimer
  clear #closedTimer
  doOpenDoor

-- It is repetetive but partial application of type alias does not compile
data IsDoorLocked ::  Type -> Exp Bool
data IsDoorClosed ::  Type -> Exp Bool
data IsDoorOpen   ::  Type -> Exp Bool
type instance Eval (IsDoorLocked selfDoorState) = IsDoor selfDoorState (DoorLocked "door")
type instance Eval (IsDoorClosed selfDoorState) = IsDoor selfDoorState (DoorClosed "door")
type instance Eval (IsDoorOpen selfDoorState)   = IsDoor selfDoorState (DoorOpen "door")

type family IsDoor selfDoor doorState :: Bool where
  IsDoor selfDoor doorState = AreEq (GetResByName "door" (UnwrapContent selfDoor)) doorState

-- :kind! Eval (IsDoorLocked (SelfDoorLocked IO))
-- :kind! Eval (IsDoor DoorOpen (SelfDoorClosed IO))
-- type CompSelfDoorLocked = StCs (DoorHnd IO) (SelfDoorLocked IO) "dr"
