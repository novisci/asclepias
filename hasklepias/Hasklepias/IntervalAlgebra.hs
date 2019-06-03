module Hasklepias.IntervalAlgebra(
  Period,
  PredicateOf,
  period,
  point,
  isPoint,
  toPeriod,
  start,
  end,
  startPoint,
  startPoints,
  endPoint,
  endPoints,
  startEndPoint,
  expandl,
  expandlPeriods,
  expandr,
  expandrPeriods,
  extentPeriod,
  collapsePeriods,
  periodGaps,
  meets,
  metBy,
  before,
  after,
  overlaps,
  overlappedBy,
  starts,
  startedBy,
  ends,
  endedBy,
  during,
  contains,
  disjoint,
  duration,
  durations
) where

{-
_TODO list_
* look at Data.Sequence (or other options) for listlike [Period] type 
   * Consider performance implications for various types:
     https://github.com/haskell-perf/sequences
   * e.g. Seq are very fast for appending but slower for filter operations
* make Period type use newtype rather than data (see Toying.hs)
  
-}


type PredicateOf a = (a -> a -> Bool) 

-- |The Periodic class specifies the functions and operators for interval
-- algebra.
class Periodic a where
    -- | Does x meet y? Does y meet x?
    meets, metBy             :: PredicateOf a
    before, after            :: PredicateOf a
    overlaps, overlappedBy   :: PredicateOf a
    mverlaps, mverlappedBy   :: PredicateOf a
    starts, startedBy        :: PredicateOf a
    ends, endedBy            :: PredicateOf a
    during, contains         :: PredicateOf a
    disjoint                 :: PredicateOf a
    duration                 :: a -> Int

    -- default function definitions
    metBy         = flip meets
    after         = flip before
    overlappedBy  = flip overlaps
    mverlappedBy  = flip mverlaps
    startedBy     = flip starts
    endedBy       = flip ends
    contains      = flip during
    disjoint x y  = (before x y) || (after x y)


-- |For now, a Period is defined in terms of Int
-- TODO: Generalize the notion of a Period to derive from arbitrary Ord types
-- see Toying.hs
data Period = 
    Point    { start :: Int, end :: Int}
  | Moment   { start :: Int, end :: Int}
  | Interval { start :: Int, end :: Int}
  deriving (Eq, Read)

instance Periodic Period where
  {- These functions assume x <= y. TODO: formalize this notion -}
  meets    x y  = (start y) == (end x)  
  before   x y  = (end x)   < (start y) 
  starts   x y  = (start x) == (start y)
  ends     x y  = (end x)   == (end y)
  during   x y  = (overlaps x y) && (end x) <= (end y)
  overlaps x y  = (start y) < (end x)
  mverlaps x y  = meets x y || overlaps x y
  duration x    = (end x) - (start x)

instance Ord Period where
  (<=) x y = (start x <= start y) || (starts x y && (end x <= end y))
  (<)  x y = (start x < start y)  || (starts x y && (end x < end y))
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance Show Period where
   show x = "(" ++ show (start x) ++ ", " ++ show (end x) ++ ")"

-- smart constructor for Period data
period :: Int -> Int -> Period
period a b 
  | b < a        = error "b < a" -- TODO: handle this in a more Haskelly way
  | a == b       = Point a b
  | b == (a + 1) = Moment a b
  | otherwise    = Interval a b

toPeriod :: (Int, Int) -> Period
toPeriod (x, y) = period x y

point :: Int -> Period
point a = Point a a

isPoint :: Period -> Bool
isPoint x = (duration x) == 0 

expandl :: Int -> Period -> Period
expandl i p = period (start p - i) (end p)
-- TODO: handle cased that i is negative

expandr :: Int -> Period -> Period
expandr i p = period (start p) (end p + i)
-- TODO: handle cased that i is negative

startPoint :: Period -> Period
startPoint x = point (start x)

startPoints :: [Period] -> [Period]
startPoints x = map startPoint x

endPoint :: Period -> Period
endPoint x = point (end x)

endPoints :: [Period] -> [Period]
endPoints x = map endPoint x

extentPeriod :: Period -> Period -> Period
extentPeriod p1 p2 = period a b 
    where a = min (start p1) (start p2)
          b = max (end p1) (end p2) 

startEndPoint :: Period -> [Period]
startEndPoint x
    | isPoint x = [x]
    | otherwise = [startPoint x, endPoint x]

durations :: [Period] -> [Int]
durations x = map duration x

(<<>>) :: [Period] -> [Period] -> [Period]
(<<>>) xl yl
   | null xl   = yl
   | null yl   = xl
   | otherwise = init xl ++ [period (start x) (end y)] ++ tailList yl
   where x = last xl
         y = head yl

(<++>) :: [Period] -> [Period] -> [Period]
(<++>) xl yl
   | null xl         = yl
   | null yl         = xl
   | x `mverlaps` y  = xl <<>> yl
   | x `before` y    = xl ++ yl
   where x = last xl
         y = head yl


(<-->) :: [Period] -> [Period] -> [Period]
(<-->) xl yl
   | null xl         = startEndPoint y
   | null yl         = startEndPoint x
   | x `mverlaps` y  = init xl ++ init (startEndPoint x) ++ tail (startEndPoint y) ++ tailList yl
   | x `before`    y = init xl ++ (startEndPoint x) <<>> (startEndPoint y) ++ tailList yl
   where x = last xl
         y = head yl


{-
TODO: Note this behavior.

s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]

*Hasklepias> periodGaps (collapsePeriods s3)
[(1, 1),(7, 13),(16, 20)]
--^^ correct
*Hasklepias> periodGaps s3
[(1, 1),(7, 16),(20, 20)]
--^^ incorrect
-}

expandrPeriods  :: Int -> [Period] -> [Period]
expandrPeriods i ps = map (\x -> expandr i x) ps

expandlPeriods  :: Int -> [Period] -> [Period]
expandlPeriods i ps = map (\x -> expandl i x) ps

collapsePeriods :: [Period] -> [Period]
collapsePeriods x = foldr (<++>) [] (map (\z -> [z]) x)

periodGaps :: [Period] -> [Period]
periodGaps x = foldr (<-->) [] (map (\z -> [z]) x)

tailList :: [a] -> [a]
tailList (_:xs)   = xs
tailList []       = []

-- Differences between all periods
-- [ duration (extentPeriod x y) | x <- [startPoint (head s3)], y <- startPoints (tail s3)]



