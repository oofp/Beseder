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

module Beseder.Resources.Timer.PaceResImpl where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Common
import           Beseder.Resources.Timer.PaceRes
import           Beseder.Misc.Prosumers.Producer
import           Beseder.Misc.Prosumers.IntervalProducer
import           Data.Function (id)
 
data PaceRes = PaceRes deriving Show

instance TaskPoster m => PaceProv m PaceRes where
  data  PaceCreated m PaceRes = PaceCreated 
  data  PaceActive m PaceRes = PaceActive (Producer m ()) 
  data  PacePaused m PaceRes = PacePaused
  data  PaceStopped m PaceRes = PaceStopped
  data  ResPar m PaceRes = PaceResPar

  createPace _ = return $ PaceCreated 
  startPace (StartPace timeoutSec) _ = activate timeoutSec
  pausePace _ (PaceActive prod) =  deactivate prod >> (return $ variantFromValue PacePaused)
  resumePace (ResumePace timeoutSec) _ = activate timeoutSec

  stopActivePace _ (PaceActive prod) = deactivate prod >> (return $ variantFromValue PaceStopped)
  stopPausedPace _ _ = return $ variantFromValue PaceStopped

  activeTransition st@(PaceActive prod) cb = (produce prod) (Just (\_ -> cb (variantFromValue st)))

  termPaused _ = return ()
  termStopped _ = return ()

deactivate :: Producer m () -> m ()
deactivate prod = produce prod Nothing 

activate :: TaskPoster m => Int -> m (V '[PaceActive m PaceRes])
activate timeoutSec = 
  (variantFromValue . PaceActive) <$> intervalProducer timeoutSec () id   


