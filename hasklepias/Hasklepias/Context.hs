module Hasklepias.Context(
  --Context,
  --CardSuitContext,
  card,
  cardContext
) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M


-- | TODO: define context and its purpose
--   key = Maybe value, 
newtype Context a b = Context (M.Map a (Maybe b))
 deriving (Eq, Ord, Show)



{-
  Context Examples
-}

data Game =
      Poker
    | BlackJack
    | War
    deriving (Show)

data Suit = 
     Spades 
   | Diamonds 
   | Hearts
   | Clubs 

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

instance Show Card where
  show x = (show $ rank x) ++ (show $ suit x)

--type PokerHand = Context (M.Map Suit Rank)
--type CardContext = Context (M.Map Game Card)
type CardContext = Context Game Card

card :: Suit -> Rank -> Card
card s v = Card {suit = s, rank = v}

cardContext :: Game -> Maybe Card -> CardContext
cardContext g (Just c) = Context $ M.singleton g (Just c)
cardContext g Nothing  = Context $ M.singleton g Nothing
