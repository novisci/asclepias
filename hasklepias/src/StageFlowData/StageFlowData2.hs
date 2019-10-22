{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StagedFlowData (
  s1
) where 

import Hasklepias.IntervalAlgebra
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.List (nub, sort)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Text.Printf
import System.CPUTime

s1 = Events [
    Event (period 0 3,  2)
  , Event (period 5 8,  2)
  , Event (period 5 10, 0)
  , Event (period 8 12, 4)]

type State = Int

makeState :: Text -> State
makeState "A" = 2
makeState "B" = 4
makeState _   = 0

none :: State
none = 0

--type Event = (Period, State)

data Tree a = 
    Nada
  | Node a (Tree a) (Tree a) 
  deriving (Eq, Show)

type EventTree = Tree Event


nTree :: a -> Tree a
nTree x = Node x Nada Nada


f :: Event -> EventTree
f e = Node e (Nada) (Nada)

g :: Event -> EventTree -> EventTree
g e (Node x _ _) = disjointEvents' x e
g e Nada = nTree e

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

h :: ([Event], [Event]) -> [Event] -> ([Event], [Event])
h (c, o:os) []        = (c++o:os, [])
h (c, [])   (e:es) = h (c, [e]) es
h (c, o:os) (e:es) 
  | p2 `after` p1  = h (c++nh, (fst $ h ([], nt) os )) es
  | p2 `metBy` p1  = h (c++nh, (fst $ h ([], nt) os )) es
  | e == o         = h (c, o:os) es
  | otherwise      = h (c, (fst $ h ([], n) os )) es
--  | otherwise      = h (c, n) es
  where n  = disjointEvents o e
        nh = [head n]
        nt = tail n
        p2 = fst $ unEvent e
        p1 = fst $ unEvent o
 

newtype Event = Event { unEvent :: (Period, State) }
  deriving (Eq, Show)

instance Ord Event where 
  (<=) (Event x) (Event y) = fst x <= fst y
  (<)  (Event x) (Event y) = fst x <  fst y
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

parseEvent :: Value -> Parser Event
parseEvent = withObject "event" $ \o -> do
    b <- o .: "s" 
    e <- o .: "e"
    s <- o .: "state"
    return $ Event (period b e, makeState s)

instance FromJSON Event where
    parseJSON = parseEvent


event :: Period -> State -> Event
event p s = Event (p, s)

newtype Events = Events { unEvents :: [Event] } deriving (Eq, Show)

instance FromJSON Events where
  parseJSON = \case
    Object o -> (o .: "events") >>= fmap Events . parseJSON
    x -> fail $ "unexpected json: " ++ show x

--- Processing ---

-- | Assuming e1 <= e2, there are 7 possible relations between e1 and e2.
--   NOTE: This currently assumes that all contained, overlapping or meeting 
--   periods of the same state have been collapsed.

newEvents :: [Event] -> [Int]
newEvents e = [(begin $ fst $ unEvent $ head e) .. (end $ fst $ unEvent $ last e)  ]


combineStates :: State -> State -> State
combineStates s1 s2
   | s1 == s2  = s1
   | otherwise = s1 + s2

disjointEvents' :: Event -> Event -> EventTree
disjointEvents' e1 e2 
   | p1 == p2           = nTree (event p1 s3)
   | p1 `starts` p2     = Node (event p1 s3) Nada (nTree (event (period p1e p2e) s2))
   | p1 `finishedBy` p2 = Node (event (period p1b p2b) s1) Nada (nTree (event p2 s3))
   | p1 `contains` p2   = Node (event p2 s3) (nTree (event (period p1b p2b) s1)) (nTree (event (period p2e p1e) s1))
   | p1 `meets` p2      = Node e1 Nada (nTree e2)
   | p1 `overlaps` p2   = Node (event (period p2b p1e) s3) (nTree (event (period p1b p2b) s1)) (nTree (event (period p1e p2e) s2))
   | p1 `before` p2     = Node (event (period p1e p2b) none) (nTree e1) (nTree e2)
   where p1  = fst $ unEvent e1
         p2  = fst $ unEvent e2
         s1  = snd $ unEvent e1
         s2  = snd $ unEvent e2
         s3  = combineStates s1 s2
         p1b = begin p1
         p2b = begin p2
         p1e = end p1
         p2e = end p2

disjointEvents :: Event -> Event -> [Event]
disjointEvents e1 e2 
   | p1 == p2           = [event p1 s3]
   | p1 `starts` p2     = [event p1 s3, event (period p1e p2e) s2 ]
   | p1 `finishedBy` p2 = [event (period p1b p2b) s1, event p2 s3]
   | p1 `contains` p2   = [event (period p1b p2b) s1, event p2 s3, event (period p2e p1e) s1]
   | p1 `meets` p2      = [e1, e2]
   | p1 `overlaps` p2   = [event (period p1b p2b) s1, event (period p2b p1e) s3, event (period p1e p2e) s2]
   | p1 `before` p2     = [e1, event (period p1e p2b) none, e2]
   where p1  = fst $ unEvent e1
         p2  = fst $ unEvent e2
         s1  = snd $ unEvent e1
         s2  = snd $ unEvent e2
         s3  = combineStates s1 s2
         p1b = begin p1
         p2b = begin p2
         p1e = end p1
         p2e = end p2

type Closed = [Event]
type Open   = [Event]
type Clopen = (Closed, Open)

{-
h :: Clopen -> Event -> [[Event]]
h c e2 
 | empty snd c = 
 | map (\e1 -> disjointEvents e1 e2 ) $ snd c 
-}


--- IO ---

inFile :: FilePath
inFile = "StageFlowData/example_data2.json"


getJSON :: IO B.ByteString
getJSON = B.readFile inFile

main :: IO ()
main = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String Events)
    case d of
        Left  err -> putStrLn err
        Right ps  -> print ps