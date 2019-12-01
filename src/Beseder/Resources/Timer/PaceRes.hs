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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.Resources.Timer.PaceRes where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef

newtype StartPace = StartPace Int deriving (Eq, Show)
data StopPace = StopPace deriving (Eq, Show)
data PausePace = PausePace deriving (Eq, Show)
newtype ResumePace = ResumePace Int deriving (Eq, Show)
  
class Monad m => PaceProv m res where
  data  PaceCreated m res 
  data  PaceActive m res 
  data  PacePaused m res 
  data  PaceStopped m res 
  data  ResPar m res 

  createPace :: MkResDef m (ResPar m res) (PaceCreated m res)
  startPace :: RequestDef m StartPace (PaceCreated m res) '[PaceActive m res]  
  pausePace :: RequestDef m PausePace (PaceActive m res) '[PacePaused m res]  
  resumePace :: RequestDef m ResumePace (PacePaused m res) '[PaceActive m res]  
  stopActivePace :: RequestDef m StopPace (PaceActive m res) '[PaceStopped m res]  
  stopPausedPace :: RequestDef m StopPace (PacePaused m res) '[PaceStopped m res]  
  activeTransition :: TransitionDef m (PaceActive m res) '[PaceActive m res]
  termPaused :: TermDef m (PacePaused m res)
  termStopped :: TermDef m (PaceStopped m res)
  
buildRes ''PaceProv

type instance TermRequest (StPaceActive m res name) = StopPace
