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
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Apps.BlackJackAutoApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Control                                               
import           Resource.BlackJackRes 
import           Game.Card
import           Control.Monad.Identity (IdentityT)
import           Data.String 

blackJackAutoApp :: 
  ( MonadIO m
  ) => CardPack -> STransApp IdentityT m NoSplitter '[()] _ _ () -- '[()] '[] ()
blackJackAutoApp cardPack = MkApp $ do
  newRes #game (newGame cardPack)
  reach @("game" :? IsGameCompleted) $ do
    invoke #game StartBlackJackGame
    invoke #game PlayerHit
    invoke #game PlayerHit
    invoke #game PlayerStand
    invoke #game DealerFirstStep
    forever (invoke #game DealerNextStep)
  invoke #game GetGameOutcome
  res <- gets #game gameOutcome
  liftIO $ putStrLn (("Game result:" :: Text) <> (result res))
  liftIO $ putStrLn (("PlayerCard:" :: Text) <> show (playerCards res))
  liftIO $ putStrLn (("DealerCard:" :: Text) <> show (dealerCards res))
  clear #game



