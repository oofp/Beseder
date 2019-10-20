{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module PureDoor where

import           Protolude  
import           Beseder.Base.Control                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.State.PureRes
import           Haskus.Utils.Variant

data PureDoor = PureDoor deriving Show
type DoorRes = PureRes PureDoor

doorRes :: DoorRes
doorRes = PureRes PureDoor

instance GetInstance DoorRes where
  getInstance = PureRes PureDoor

data ClosedDoor = ClosedDoor deriving Show
data OpenedDoor = OpenedDoor deriving Show
data LockedDoor = LockedDoor deriving Show

data LockDoor = LockDoor deriving Show
data UnlockDoor = UnlockDoor deriving Show
data OpenDoor = OpenDoor deriving Show
data CloseDoor = CloseDoor deriving Show

instance GetInstance CloseDoor where
  getInstance = CloseDoor
instance GetInstance UnlockDoor where
  getInstance = UnlockDoor
instance GetInstance LockDoor where
  getInstance = LockDoor
instance GetInstance OpenDoor where
  getInstance = OpenDoor
      
            
instance MkPureRes PureDoor  where
  type PureResInitState PureDoor = ClosedDoor
  mkPureRes PureDoor = ClosedDoor

instance Op LockDoor ClosedDoor where
  type OpResults LockDoor ClosedDoor = '[LockedDoor]
  opReq LockDoor ClosedDoor = variantFromValue LockedDoor

instance Op UnlockDoor LockedDoor where
  type OpResults UnlockDoor LockedDoor = '[ClosedDoor]
  opReq UnlockDoor LockedDoor = variantFromValue ClosedDoor

instance Op OpenDoor ClosedDoor where
  type OpResults OpenDoor ClosedDoor = '[OpenedDoor]
  opReq OpenDoor ClosedDoor = variantFromValue OpenedDoor
  
instance Op CloseDoor OpenedDoor where
  type OpResults CloseDoor OpenedDoor = '[ClosedDoor]
  opReq CloseDoor OpenedDoor = variantFromValue ClosedDoor
    
type DoorClosed name = PureSt ClosedDoor name 
type DoorOpen name = PureSt OpenedDoor name 
type DoorLocked name = PureSt LockedDoor name 

