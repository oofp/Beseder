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
import           Beseder.Misc.Misc
import           Beseder.Resources.State.PureRes

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

instance OpOne LockDoor ClosedDoor where
  type OpResult LockDoor ClosedDoor = LockedDoor
  opReqOne LockDoor ClosedDoor = LockedDoor

instance OpOne UnlockDoor LockedDoor where
  type OpResult UnlockDoor LockedDoor = ClosedDoor
  opReqOne UnlockDoor LockedDoor = ClosedDoor

instance OpOne OpenDoor ClosedDoor where
  type OpResult OpenDoor ClosedDoor = OpenedDoor
  opReqOne OpenDoor ClosedDoor = OpenedDoor
  
instance OpOne CloseDoor OpenedDoor where
  type OpResult CloseDoor OpenedDoor = ClosedDoor
  opReqOne CloseDoor OpenedDoor = ClosedDoor
    
