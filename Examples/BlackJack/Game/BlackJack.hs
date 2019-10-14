{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}  
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--https://www.blackjack.org/blackjack-rules/

module Game.BlackJack 
    ( createCardPack
    , initGame
    , playerHit
    , playerStand
    , dealerFirstStep
    , dealerNextStep
    , getPlayerCards
    , getDealerCards
    , getGameResult
    , getGameResultText
    , GameResult (..)
    , AllGameResults
    , PlayerTurn
    , GameOver 
    , DealerFirstTurn
    , DealerNextTurn
    , CardPack
    , DealerStepOutcome
    , GameState
    , GameStatus (..)
    , IsGameOver
    -- , GameSummary (..)
    ) where

import           Protolude
import           Game.Card
import           Haskus.Utils.Variant

-- ADT to decribe possible game result
data GameResult 
    = BlackJack
    | PlayerWon
    | DealerWon
    | Push
    | PlayerBusted
    | DealerBusted
    deriving (Show,Enum,Bounded,Typeable)

-- ADT to describe possible stages of the game    
data GameStatus 
    = PlayerTurnStatus
    | DealerFirstTurnStatus   
    | DealerNextTurnStatus
    | GameOverStatus GameResult

-- Record that describe game data (notice that GameStatus is type parameter, not data parameter)    
data GameState (gameStatus::GameStatus) (playerStatus::HandStatus) (dealerStatus::HandStatus) = GameState         
    { playerHand :: PlayerHand playerStatus
    , dealerHand :: PlayerHand dealerStatus
    , pack :: CardPack
    } deriving Show

-- define allowed hand states for all game results    
type family PlayerHandForRes (gameRes :: GameResult) :: HandStatus where
  PlayerHandForRes BlackJack = Good
  PlayerHandForRes PlayerWon = Good
  PlayerHandForRes DealerWon = Good
  PlayerHandForRes Push = Good
  PlayerHandForRes PlayerBusted = Busted
  PlayerHandForRes DealerBusted = Good
type family DealerHandForRes (gameRes :: GameResult) :: HandStatus where
  PlayerHandForRes BlackJack = OneCard
  DealerHandForRes PlayerWon = Good
  DealerHandForRes DealerWon = Good
  DealerHandForRes Push = Good
  DealerHandForRes PlayerBusted = OneCard
  DealerHandForRes DealerBusted = Busted
 
-- define type aliases for all states 
type PlayerTurn = GameState 'PlayerTurnStatus 'Good 'OneCard     
type DealerFirstTurn = GameState 'DealerFirstTurnStatus 'Good 'OneCard     
type DealerNextTurn = GameState 'DealerNextTurnStatus 'Good 'Good
-- use type family to set hand states for supported game results
type GameOver (res :: GameResult) = GameState (GameOverStatus res) (PlayerHandForRes res) (DealerHandForRes res)     

type family IsGameOver st :: Bool where
  IsGameOver (GameState (GameOverStatus _) _ _) = True
  IsGameOver _ = False

-- define type level list for all possible game over states 
-- (boilerplate could be (hopefully) avoided by using singleton package)
type AllGameResults = 
  '[ GameOver 'BlackJack
   , GameOver 'PlayerWon
   , GameOver 'DealerWon
   , GameOver 'Push
   , GameOver 'PlayerBusted
   , GameOver 'DealerBusted
   ]

-- initialize game state and all possible state transition done safely   
initGame :: CardPack -> Either (GameOver BlackJack) PlayerTurn 
initGame cardPack = 
    let ((card1,card2,card3), newPack) = runState (liftM3 (,,) getCard getCard getCard) cardPack 
    in 
      if gotBlackJack card1 card2 card3 
        then Left $ initGameState card1 card2 card3 newPack 
        else Right $ initGameState card1 card2 card3 newPack

initGameState card1 card2 card3 newPack = GameState (initWithTwoCards card1 card2) (initWithOneCard card3) newPack          

-- state transition functions have signature
-- request :: initState -> combinationOfAllowedOutcomes (one, Either or Variant)
-- when Either is used type of new game state can be inferred
-- if we try to use 'hand' at wrong state to create game state we get compile error
playerHit :: PlayerTurn -> Either (GameOver PlayerBusted) PlayerTurn
playerHit (GameState {..}) = 
    let (card, newPack) = runState getCard pack 
    in case addCardToHand card playerHand of
        Left bustedHand -> Left $ GameState bustedHand dealerHand newPack
        Right goodHand -> Right $ GameState goodHand dealerHand newPack  

playerStand :: PlayerTurn -> DealerFirstTurn
playerStand (GameState {..}) = GameState playerHand dealerHand pack

type DealerStepOutcome =
 '[ DealerNextTurn 
  , GameOver PlayerWon
  , GameOver DealerWon
  , GameOver Push
  , GameOver DealerBusted
  ]

dealerFirstStep :: DealerFirstTurn -> V DealerStepOutcome 
dealerFirstStep  = dealerStep
  
dealerNextStep :: DealerNextTurn -> V DealerStepOutcome 
dealerNextStep  = dealerStep
 
-- when type of game state cannot be inferred we pass its type as parameter using 
-- TypeApplications , hence forall is needed (gameStatus does not appear as term parameter)
-- {-# LANGUAGE TypeApplications      #-}

gameState :: forall gameStatus ph dh.
              (PlayerHand ph, PlayerHand dh, CardPack)
              -> GameState gameStatus ph dh        
gameState (ph,dh,pack) = GameState  ph dh pack

gameOver :: 
  forall gameRes playerHand dealerHand. 
  ( PlayerHandForRes gameRes ~ playerHand
  , DealerHandForRes gameRes ~ dealerHand
  ) => (PlayerHand playerHand, PlayerHand dealerHand, CardPack)
              -> GameState (GameOverStatus gameRes) playerHand dealerHand        
gameOver = gameState @(GameOverStatus gameRes)

dealerStep :: (CanAddCard dealerStatus  ~ 'True) => GameState gameStatus 'Good dealerStatus -> V DealerStepOutcome 
dealerStep (GameState {..}) =     
  let (card, newPack) = runState getCard pack
  in case addCardToHand card dealerHand of
    Left bustedHand -> toVariant $ gameOver @DealerBusted (playerHand,bustedHand,newPack) 
    Right goodHand -> 
      let 
        dealerScore = handBestScore goodHand
        hp = (playerHand, goodHand, newPack)
      in 
        if dealerScore < 17
          then toVariant $ gameState @'DealerNextTurnStatus hp-- toVariantAt @0 $ gameState hp
          else 
            let playerScore = handBestScore playerHand
            in case compare playerScore dealerScore of
              GT -> toVariant $ gameOver @'PlayerWon hp 
              LT -> toVariant $ gameOver @'DealerWon hp 
              EQ -> toVariant $ gameOver @'Push hp

-- more utilites function              
getPlayerCards :: GameState gameStatus handStatus dealerStatus -> HandCards
getPlayerCards = getHandCards . playerHand
getDealerCards :: GameState gameStatus playerStatus handStatus -> HandCards
getDealerCards = getHandCards . dealerHand

getGameResult :: (Typeable res) => GameOver (res :: GameResult) -> Proxy (res :: GameResult)
getGameResult _ = Proxy

getGameResultText :: (Typeable res) => GameOver (res :: GameResult) -> Text
getGameResultText = show . typeRep . getGameResult

{-
-- use type class to get game summary from Variant with possible game results
class GameSummary xs where
  getGameSummary :: V xs  -> (HandCards,HandCards,Text)

instance GameSummary '[] where
  getGameSummary var = undefined

instance (x ~ GameOver res, Typeable res, GameSummary xs) => GameSummary  (x ': xs) where
  getGameSummary var = case popVariantHead var of 
    Right gmOver -> (getPlayerCards gmOver, getDealerCards gmOver, getGameResultText gmOver)
    Left v_xs -> getGameSummary v_xs
-}
