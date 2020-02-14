{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Resource.BlackJackRes where

import           Haskus.Utils.Variant
import           Protolude
import           Beseder.Base.Common                                               
import           Beseder.Resources.State.PureRes
import           Game.BlackJack
import           Game.Card

newGame :: CardPack -> PureRes CardPack
newGame = PureRes

newtype BlackJackGamePack = BlackJackGamePack CardPack deriving (Eq, Show)

instance MkPureRes CardPack where
  type PureResInitState CardPack = BlackJackGamePack 
  mkPureRes = BlackJackGamePack

data StartBlackJackGame = StartBlackJackGame deriving (Eq, Show)
data PlayerHit = PlayerHit deriving (Eq, Show)
data PlayerStand = PlayerStand deriving (Eq, Show)
data DealerFirstStep = DealerFirstStep deriving (Eq, Show)
data DealerNextStep = DealerNextStep deriving (Eq, Show)

data GetGameOutcome = GetGameOutcome deriving (Show, Eq)
data GameOutcome = GameOutcome
    { dealerCards :: HandCards
    , playerCards :: HandCards
    , result :: Text
    } deriving (Show, Eq)

instance Op StartBlackJackGame  BlackJackGamePack where
    type OpResults StartBlackJackGame  BlackJackGamePack = '[PlayerTurn, (GameOver BlackJack)]
    opReq StartBlackJackGame (BlackJackGamePack cardPack) = variantFromEither $ initGame cardPack  

instance Op PlayerHit PlayerTurn where
    type OpResults PlayerHit  PlayerTurn = '[PlayerTurn, (GameOver PlayerBusted)]
    opReq PlayerHit gameState = variantFromEither $ playerHit gameState  
    
instance Op PlayerStand PlayerTurn where
    type OpResults PlayerStand  PlayerTurn = '[DealerFirstTurn]
    opReq PlayerStand gameState = variantFromValue $ playerStand gameState  
    
instance Op DealerFirstStep DealerFirstTurn where
    type OpResults DealerFirstStep  DealerFirstTurn = DealerStepOutcome
    opReq DealerFirstStep  gameState =  dealerFirstStep gameState  
    
instance Op DealerNextStep DealerNextTurn where
    type OpResults DealerNextStep  DealerNextTurn = DealerStepOutcome
    opReq DealerNextStep  gameState = dealerNextStep gameState  

instance (gameState ~ GameOver res, Typeable res) => Op GetGameOutcome gameState where
    type OpResults GetGameOutcome gameState = '[GameOutcome]
    opReq GetGameOutcome gameState = variantFromValue (
        GameOutcome (getDealerCards gameState) (getPlayerCards gameState) (getGameResultText gameState))  
    
type StBlackJackGame name = St (PSt BlackJackGamePack) name
type StPlayerTurn name =St (PSt PlayerTurn) name
type StDealerFirstTurn name = St (PSt DealerFirstTurn) name    
type StDealerNextTurn name = St (PSt DealerNextTurn) name    
type StGameOver gameResult name = St (PSt (GameOver gameResult)) name    

cardsOfPlayer :: St (PSt (GameState gameStatus playerStatus handStatus)) name -> Game.Card.HandCards
cardsOfPlayer st = cardsOf getPlayerCards st

cardsOfDealer :: St (PSt (GameState gameStatus playerStatus handStatus)) name  -> Game.Card.HandCards
cardsOfDealer st = cardsOf getDealerCards st

cardsOf :: (t1 -> t2) -> St (PSt t1) name -> t2
cardsOf getCards (St (PSt gameState)) = getCards gameState 

gameResult :: (Typeable gameResult) => StGameOver gameResult name -> Text
gameResult (St (PSt gameState)) = getGameResultText gameState 

data IsGameCompleted :: Type -> Exp Bool
type instance Eval (IsGameCompleted (St (PSt gameState) name)) = IsGameOver gameState
 
gameOutcome :: St (PSt GameOutcome) name -> GameOutcome 
gameOutcome (St (PSt outcome)) = outcome 

