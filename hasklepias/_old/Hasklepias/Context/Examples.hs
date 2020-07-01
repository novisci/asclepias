module Hasklepias.Context.Examples(
  card,
  cardContext,
  pokerHand,
  roundContext,
  warRound,
  pokerRound
) where

import Hasklepias.Context
import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
{-
  Context Examples
-}

-- Game + Card

data Game =
      Poker
    | BlackJack
    | War
    deriving (Eq, Show)

data Suit = 
     Spades 
   | Diamonds 
   | Hearts
   | Clubs 
   deriving (Eq)

instance Show Suit where
   show Spades   = "\9824"
   show Diamonds = "\9830"
   show Hearts   = "\9829"
   show Clubs    = "\9827"

data Rank = 
     Ace 
   | Two 
   | Three
   | Four
   | Five 
   | Six 
   | Seven 
   | Eight 
   | Nine 
   | Ten 
   | Jack 
   | Queen 
   | King
   deriving (Eq)

instance Show Rank where
    show Ace   = "A" 
    show Two   = "2" 
    show Three = "3"
    show Four  = "4"
    show Five  = "5" 
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"

data Card = Card { 
     suit :: Suit
   , rank :: Rank }
   deriving (Eq)

instance Show Card where
  show x = (show $ rank x) ++ (show $ suit x)

data GameCard = GameCard {
      getGame :: Game
    , getCard :: Maybe Card}


instance Show GameCard where
   show x = (show g) ++ " " ++ (show c)
    where g = getGame x
          c = getCard x


type CardContext = Context GameCard

card :: Suit -> Rank -> Card
card s v = Card {suit = s, rank = v}

gameCardContextualizer :: (Game -> Maybe Card -> CardContext)
gameCardContextualizer g c = contextns "gameCard" $ GameCard g c

cardContext :: Game -> Maybe Card -> CardContext
cardContext g (Just c) = gameCardContextualizer g $ Just c
cardContext g Nothing  = gameCardContextualizer g $ Nothing

-- Game + Hand
data Player =
    A
  | B
  | C
  deriving (Eq, Ord, Show)

type Cards = [Card]

newtype Hand = Hand { unHand :: Cards } 
  deriving (Eq)

instance Show Hand where
  show x = show $ unHand x

type PokerHand = Maybe Hand

pokerHand :: Maybe Cards -> PokerHand
pokerHand (Just l) 
    | length l == 5 = Just $ Hand l
    | otherwise     = Nothing
pokerHand Nothing   = Nothing

data GameRound = GameRound {
   pullGame :: Game,
   getHands :: M.Map Player (Maybe Hand) }
   deriving (Show, Eq) 

type RoundContext = Context GameRound


roundContext :: Game -> [(Player, Maybe Hand)] -> RoundContext
roundContext g l = contextns "gameRound" $ GameRound g (M.fromList l)

warRound :: RoundContext
warRound = roundContext War [ (A, Just $ Hand [(card Diamonds Two)])
                            , (B, Nothing)
                            , (C, Just $ Hand [(card Clubs Ace)])]

pokerRound :: RoundContext
pokerRound = roundContext Poker 
   [ (A, pokerHand $ Just 
      [ card Diamonds Two
      , card Diamonds Ace
      , card Diamonds Queen
      , card Clubs Seven
      , card Spades Ace])
    , (B, pokerHand $ Just 
      [ card Clubs Two])
    , (C, pokerHand $ Just 
      [ card Hearts Queen
      , card Hearts Jack
      , card Hearts Ten
      , card Clubs Five
      , card Spades Seven])]
