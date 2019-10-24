{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module StagedFlowData (

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

type State = [Bool]

none :: State
none = [False, False, False]

makeState :: Text -> State
makeState "A" = [True, False, False]
makeState "B" = [False, True, False]
makeState "C" = [False, False, True]
makeState _   = none

-- | Combines two States to create a new State

combineStates :: State -> State -> State
combineStates = zipWith (||)

--- Event ---

-- TODO: eventually use Hasklepias' event type
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

-- | Convert an ordered sequence of events that may have any interval
--   relation (before, starts, etc) into a sequence of sequentially 
--   meeting events. That is, a sequence where one the end of one event
--   meets the beginning of the subsequent event. The states of the input
--   events are combined using the combineStates function. Internally this 
--   uses the function h.

disjoinEvents :: [Event] -> [Event]
disjoinEvents = h ([], [])

-- | The internal function for converting a non-disjoint, ordered sequence of
--   events into a disjoint, ordered sequence of events. The function operates
--   by recursion on a pair of events and the input events. The first of the 
--   is the accumulator set -- the disjoint events that need no longer be 
--   compared to input events. The second of the pair are disjoint events that
--   still need to be compared to be input events. 

h :: ([Event], [Event]) -> [Event] -> [Event]
h (c, o:os) []     = c++o:os       -- the "final" pattern
h (c, [])   []     = c             -- another "final" pattern 
h (c, [])   (e:es) = h (c, [e]) es -- the "initialize" pattern
h (c, o:os) (e:es)                 -- the "operating" patterns 

  -- If input event is equal to the first comparator, skip the comparison.
  | e == o    = h (c, o:os) es     

  -- Points are a pain in the arse. This pattern handles a corner case 
  -- wherein the period of o is a point and the states of o, e, and o+e
  -- are the same. There is probably a way to handle this more elegantly,
  -- but word of warning: here be dragons of infinite recursion.
  -- Note also, the result of this pattern is the same as the otherwise 
  -- pattern. But this case also matches the next pattern, and points that
  -- enter that pattern will lead to infinite recursion. Hence, we catch
  -- these points here first.
  | isPoint p1 && sSo && sSe
              = h (c,  h ([], n) os ) es

  -- If the period of o is either before or meets the period of e, then 
  -- the first of the combined events can be put into the accumulator. 
  -- That is, since the inputs events are ordered, once the beginning of o 
  -- is before or meets e, then we are assured that all periods up to the 
  -- beginning of o are fully disjoint and subsequent input events will 
  -- not overlap these in any way.
  | p1 `before` p2 || p1 `meets`  p2 || isPoint p2
              = h (c++nh, h ([], nt) os ) es

  -- The standard recursive operation.
  | otherwise = h (c,  h ([], n) os ) es

  where n  = combineEvents e1 e2 
        -- taking min max because in certain cases o e can be out of order
        e1 = min o e
        e2 = max o e
        nh = [head n]
        nt = tail n
        p2 = fst $ unEvent e
        p1 = fst $ unEvent o
        sSe = (snd $ unEvent e) == (snd $ unEvent $ head n)
        sSo = (snd $ unEvent o) == (snd $ unEvent $ head n)

-- | Takes two *ordered* events, x <= y, and "disjoins" them in the case that the
--   two events have different states, creating a sequence (list) of new events that 
--   sequentially meet one another. Since x <= y, there are 7 possible interval
--   relations between x and y. If the states of x and y are equal and x is not 
--   before y, then x and y are combined into a single event.

combineEvents :: Event -> Event -> [Event]
combineEvents x y
   |           p1 == p2           = [ ev p1 s3 ]
   |           p1 `before` p2     = [ x, evp e1 b2 none, y ]
   | not sS && p1 `starts` p2     = [ ev p1 s3, evp e1 e2 s2 ]
   | not sS && p1 `finishedBy` p2 = [ evp b1 b2 s1, ev p2 s3 ]
   | not sS && p1 `contains` p2   = [ evp b1 b2 s1, ev p2 s3, evp e2 e1 s1 ]
   | not sS && p1 `meets` p2      = [ x, y ]
   | not sS && p1 `overlaps` p2   = [ evp b1 b2 s1, evp b2 e1 s3, evp e1 e2 s2 ]
   | otherwise                    = [ evp b1 e2 s1 ]      
   where p1 = fst $ unEvent x
         p2 = fst $ unEvent y
         s1 = snd $ unEvent x
         s2 = snd $ unEvent y
         s3 = combineStates s1 s2
         b1 = begin p1
         b2 = begin p2
         e1 = end p1
         e2 = end p2
         sS = (s1 == s2)

{--- Testing ---}

pullPeriods :: [Event] -> [Period]
pullPeriods = map (\x -> fst $ unEvent x)

checkSeqStates :: [Period] -> Bool
checkSeqStates l = fst $ foldl (\x y -> (fst x && snd x `meets` y , y)) (True, period (-1) 0) l

s0 = 
  [ evp 0 10  [True, False, False]
  , evp 2 10  [True, False, False]
  , evp 5 8   [False, True, False]
  , evp 6 9   [False, True, False]]


s1 =
  [ evp 0 3  [True, False, False]
  , evp 5 8  [True, False, False]
  , evp 5 10 [True, False, False]
  , evp 8 12 [True, False, False]]

s2 =
  [ evp 0 3  [True, False, False]
  , evp 5 8  [True, False, False]
  , evp 5 10 [True, False, False]
  , evp 8 12 [True, False, False]]

s3 =
  [ evp 0 0 [True, False, False]
  , evp 0 1 [True, False, False]
  , evp 0 2 [True, False, False]
  , evp 0 3 [True, False, False]]

s4 =
  [ evp 0 5 [True, False, False]
  , evp 1 5 [True, False, False]
  , evp 2 5 [True, False, False]
  , evp 3 5 [True, False, False]]

s5 =
  [ evp 0 5 [True, False, False]
  , evp 1 5 [True, False, False]
  , evp 3 3 [False, True, False]
  , evp 3 5 [True, False, False]]

{--- IO ---}

inFile :: FilePath
inFile = "StageFlowData/example_data2.json"


getJSON :: IO B.ByteString
getJSON = B.readFile inFile

main :: IO ()
main = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String Events)
    case d of
        Left  err -> putStrLn err
        Right ps  -> print $ disjoinEvents $ unEvents ps