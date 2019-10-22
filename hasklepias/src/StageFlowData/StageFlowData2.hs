{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StagedFlowData (
  s1
) where 

import Hasklepias.IntervalAlgebra
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.List (nub)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Text.Printf
import System.CPUTime

--- Data types ---

type State = Int

makeState :: Text -> State
makeState "A" = 2
makeState "B" = 4
makeState _   = 0

none :: State
none = 0

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

eventp :: Int -> Int -> State -> Event
eventp b e s = event (period b e) s

newtype Events = Events { unEvents :: [Event] } deriving (Eq, Show)

instance FromJSON Events where
  parseJSON = \case
    Object o -> (o .: "events") >>= fmap Events . parseJSON
    x -> fail $ "unexpected json: " ++ show x

--- Processing ---

h :: ([Event], [Event]) -> [Event] -> ([Event], [Event])
h (c, o:os) []     = (c++o:os, [])
h (c, [])   (e:es) = h (c, [e]) es
h (c, o:os) (e:es) 
  | p1 `before` p2 || 
    p1 `meets`  p2   = h (c++nh, (fst $ h ([], nt) os )) es
  | e == o           = h (c, o:os) es
  | otherwise        = h (c, (fst $ h ([], n) os )) es
  where n  = disjointEvents e1 e2
        e1 = min o e
        e2 = max o e
        nh = [head n]
        nt = tail n
        p2 = fst $ unEvent e
        p1 = fst $ unEvent o

combineStates :: State -> State -> State
combineStates s1 s2
   | s1 == s2  = s1
   | otherwise = s1 + s2

-- | Assuming e1 <= e2, there are 7 possible relations between e1 and e2.
--   NOTE: This currently assumes that all contained, overlapping or meeting 
--   periods of the same state have been collapsed.

disjointEvents :: Event -> Event -> [Event]
disjointEvents e1 e2 
   |           p1 == p2           = [event p1 s3]
   |           p1 `before` p2     = [e1, eventp p1e p2b none, e2]
   | not sS && p1 `starts` p2     = [event p1 s3, eventp p1e p2e s2 ]
   | not sS && p1 `finishedBy` p2 = [eventp p1b p2b s1, event p2 s3]
   | not sS && p1 `contains` p2   = [eventp p1b p2b s1, event p2 s3, eventp p2e p1e s1]
   | not sS && p1 `meets` p2      = [e1, e2]
   | not sS && p1 `overlaps` p2   = [eventp p1b p2b s1, eventp p2b p1e s3, eventp p1e p2e s2]
   | otherwise                    = [eventp p1b p2e s1]      
   where p1  = fst $ unEvent e1
         p2  = fst $ unEvent e2
         s1  = snd $ unEvent e1
         s2  = snd $ unEvent e2
         s3  = combineStates s1 s2
         p1b = begin p1
         p2b = begin p2
         p1e = end p1
         p2e = end p2
         sS  = s1 == s2

--- Testing ---

s1 =
  [ eventp 0 3 2
  , eventp 5 8 2
  , eventp 5 10 1
  , eventp 8 12 4]

s2 =
  [ eventp 0 3 2
  , eventp 5 8 2
  , eventp 5 10 1
  , eventp 8 12 1]

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