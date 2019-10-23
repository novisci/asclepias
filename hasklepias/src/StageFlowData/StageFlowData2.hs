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

{--- Data types ---}

--- State ---

type State = Int

makeState :: Text -> State
makeState "A" = 2
makeState "B" = 4
makeState _   = 0

none :: State
none = 0

-- | Combines two States to create a new State

combineStates :: State -> State -> State
combineStates s1 s2
   | s1 == s2  = s1
   | otherwise = s1 + s2

--- Event ---

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

ev :: Period -> State -> Event
ev p s = Event (p, s)

evp :: Int -> Int -> State -> Event
evp b e s = ev (period b e) s

--- Events ---

newtype Events = Events { unEvents :: [Event] } deriving (Eq, Show)

instance FromJSON Events where
  parseJSON = \case
    Object o -> (o .: "events") >>= fmap Events . parseJSON
    x -> fail $ "unexpected json: " ++ show x

{--- Processing ---}

-- | TODO: thoroughly describe this internal function

h :: ([Event], [Event]) -> [Event] -> [Event]
h (c, o:os) []     = c++o:os
h (c, [])   []     = c
h (c, [])   (e:es) = h (c, [e]) es
h (c, o:os) (e:es) 
  | isPoint p1     = h (c,  h ([], n) os ) es
  | p1 `before` p2 || p1 `meets`  p2 || isPoint p2
                   = h (c++nh, h ([], nt) os ) es
  | e == o         = h (c, o:os) es
  | otherwise      = h (c,  h ([], n) os ) es
  where n  = combineEvents e1 e2
        e1 = min o e
        e2 = max o e
        nh = [head n]
        nt = tail n
        p2 = fst $ unEvent e
        p1 = fst $ unEvent o

-- | TODO:

g :: [Event] -> [Event]
g = h ([], [])

-- | Takes two *ordered* events, e1 <= e2, and "disjoins" them in the case that the
--   two events have different states, creating a sequence (list) of new events that 
--   sequentially meet one another. Since e1 <= e2, there are 7 possible interval
--   relations between e1 and e2. If the states of e1 and e2 are equal and e1 is not 
--   before e2, then e1 and e2 are combined into a single event.

combineEvents :: Event -> Event -> [Event]
combineEvents e1 e2 
   |           p1 == p2           = [  ev p1 s3 ]
   |           p1 `before` p2     = [  e1
                                     , evp p1e p2b none, e2 ]
   | not sS && p1 `starts` p2     = [  ev p1 s3
                                     , evp p1e p2e s2 ]
   | not sS && p1 `finishedBy` p2 = [  evp p1b p2b s1
                                     , ev p2 s3 ]
   | not sS && p1 `contains` p2   = [  evp p1b p2b s1
                                     , ev p2 s3
                                     , evp p2e p1e s1 ]
   | not sS && p1 `meets` p2      = [  e1
                                     , e2 ]
   | not sS && p1 `overlaps` p2   = [  evp p1b p2b s1
                                     , evp p2b p1e s3
                                     , evp p1e p2e s2 ]
   | otherwise                    = [  evp p1b p2e s1 ]      
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

s0 = 
  [ evp 0 10  2
  , evp 2 10  2
  , evp 5 8   1
  , evp 6 9   4]

s1 =
  [ evp 0 3  2
  , evp 5 8  2
  , evp 5 10 1
  , evp 8 12 4]

s2 =
  [ evp 0 3  2
  , evp 5 8  2
  , evp 5 10 1
  , evp 8 12 1]

s3 =
  [ evp 0 0 1
  , evp 0 1 1
  , evp 0 2 1
  , evp 0 3 1]

s4 =
  [ evp 0 5 1
  , evp 1 5 1
  , evp 2 5 1
  , evp 3 5 1]

s5 =
  [ evp 0 5 1
  , evp 1 5 2
  , evp 3 3 6
  , evp 3 5 6]

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