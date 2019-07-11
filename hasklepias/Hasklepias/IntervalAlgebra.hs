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
  extentPeriods,
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
  durations,
  pairPeriods,
  comparePeriodPairs,
  comparePeriodPairsList
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

    -- | Is x before y? Is x after y?
    before, after            :: PredicateOf a
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy   :: PredicateOf a
    
    -- | Does x meet or overlap y? Is x met or overlapped by y?
    mverlaps, mverlappedBy   :: PredicateOf a
    
    -- | Does x start y? Is x started by y?
    starts, startedBy        :: PredicateOf a
    
    -- | Does x end y? Is x ended by y?
    ends, endedBy            :: PredicateOf a
    
    -- | Is x during y? Does x contain y?
    during, contains         :: PredicateOf a
    
    -- | Are x and y disjoint?
    disjoint                 :: PredicateOf a
    
    -- | What is the duration of x?
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
  before   x y  = (end x)   <  (start y) 
  starts   x y  = (start x) == (start y)
  ends     x y  = (end x)   == (end y)
  during   x y  = (overlaps x y) && (end x) <= (end y)
  overlaps x y  = not ( before x y || after x y)
  mverlaps x y  = meets x y || overlaps x y
  duration x    = (end x) - (start x)

instance Ord Period where
  (<=) x y = (start x <= start y) || (starts x y && (end x <= end y))
  (<)  x y = (start x < start y)  || (starts x y && (end x < end y))
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance Show Period where
   show x = "(" ++ show (start x) ++ ", " ++ show (end x) ++ ")"

type PeriodPairs = [(Period, Period)]
type PeriodComparator a = (Period -> Period -> a)

{-
 Functions for basic manipulations of a single period or element-wise in a 
 list of Periods
-}

-- |Constructor for Period data from Int
period :: Int -> Int -> Period
period a b
  | b < a        = error "b < a" -- TODO: handle this in a more Haskelly way
  | b == a       = Point a a
  | b == (a + 1) = Moment a (a + 1)
  | otherwise    = Interval a b

-- |Creates point from a single Int
point :: Int -> Period
point a = Point a a

-- |Converts a pairs of Int to a Period
toPeriod :: (Int, Int) -> Period
toPeriod x = uncurry period x

-- | Expands a period to left by l and to the right by r
-- TODO: handle cases that l or r are negative
expand :: Int -> Int -> Period -> Period
expand l r p = period ((start p) - l) ((end p) + r)

-- | Expands a period to left by i
expandl :: Int -> Period -> Period
expandl i p = expand i 0 p

-- | Expands each period in a list to the left by i
expandlPeriods  :: Int -> [Period] -> [Period]
expandlPeriods i ps = map (expandl i) ps

-- | Expands a period to right by i
expandr :: Int -> Period -> Period
expandr i p = expand 0 i p

-- | Expands each period in a list to the right by i
expandrPeriods  :: Int -> [Period] -> [Period]
expandrPeriods i ps = map (expandr i) ps

-- | Contract a period to a Point at its start
startPoint :: Period -> Period
startPoint x = point (start x)

-- | Contract each period in the list to its start point
startPoints :: [Period] -> [Period]
startPoints x = map startPoint x

-- | Contract a period to a Point at its end
endPoint :: Period -> Period
endPoint x = point (end x)

-- | Contract each period in the list to its end point
endPoints :: [Period] -> [Period]
endPoints x = map endPoint x

-- | Form a list of two points from the start and end of a period. If x is 
--   already a point, returns [x].
startEndPoint :: Period -> [Period]
startEndPoint x
    | isPoint x = [x]
    | otherwise = [startPoint x, endPoint x]

{-
 Functions for comparing and combining multiple Periods
-}

-- | From a pair of periods form a new period from the min of the start points
--   to the max of the end points.
extentPeriod :: Period -> Period -> Period
extentPeriod p1 p2 = period a b 
    where a = min (start p1) (start p2)
          b = max (end p1)   (end p2)

-- | Form the extentPeriod for each element in a PeriodPairs.
extentPeriods :: PeriodPairs -> [Period]
extentPeriods x = map (\z -> extentPeriod (fst z) (snd z)) x

-- | Link two lists of Periods by creating a linking period from the start of 
--   the last period in the first list and the end of the first period in the 
--   second list
(<<>>) :: [Period] -> [Period] -> [Period]
(<<>>) xl yl
   | null xl   = yl
   | null yl   = xl
   | otherwise = init xl ++ [period (start x) (end y)] ++ tailList yl
   where x = last xl
         y = head yl

-- | Collapse two lists of Periods such that if the last period of the first 
--   list and the first period of the second overlap they are linked by `<<>>`. 
--   Otherwise, the lists are concatenated.
(<++>) :: [Period] -> [Period] -> [Period]
(<++>) xl yl
   | null xl         = yl
   | null yl         = xl
   | x `mverlaps` y  = xl <<>> yl
   | x `before` y    = xl ++ yl
   where x = last xl
         y = head yl

-- | TODO
--   Note this behavior on overlapping periods:
--   s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]
--   *Hasklepias> periodGaps s3
--   [(1, 1),(7, 16),(20, 20)]
--
--   But for non overlapping periods:
--   *Hasklepias> periodGaps (collapsePeriods s3)
--   [(1, 1),(7, 13),(16, 20)]
(<-->) :: [Period] -> [Period] -> [Period]
(<-->) xl yl
   | null xl         = startEndPoint y
   | null yl         = startEndPoint x
   | x `mverlaps` y  = init xl ++ 
                       init (startEndPoint x) ++ tail (startEndPoint y) ++
                       tailList yl
   | x `before` y    = init xl ++ 
                       (startEndPoint x) <<>> (startEndPoint y) ++ 
                       tailList yl
   where x = last xl
         y = head yl

-- | Traverses over a list of periods collapsing the periods by `<++>` to create
--   a list of non-overlapping periods.
collapsePeriods :: [Period] -> [Period]
collapsePeriods x = foldr (<++>) [] (map (\z -> [z]) x)

-- | TODO
periodGaps :: [Period] -> [Period]
periodGaps x = foldr (<-->) [] (map (\z -> [z]) x)

-- | Builds a list of lists of pairs of each successive head Period with the 
--   remaining tail Periods after applying headf to the head Period and 
--   tailf to the tail Periods. Returns a list of PeriodPairs of length n - 1, 
--   where n is the length of the input list. 
pairPeriods :: (Period -> Period) -> ([Period] -> [Period]) -> [Period] -> [PeriodPairs]
pairPeriods headf tailf (x:xs) 
  | null xs    = []
  | otherwise = [[ (s, e) | s <- [headf x], e <- tailf xs]] ++ 
                 pairPeriods headf tailf xs

{-
  Functions for deriving new information from a Period, pairs for Periods, or
  lists of Periods
-}

-- | 
comparePeriodPairs :: PeriodComparator a -> PeriodPairs -> [a]
comparePeriodPairs f x = map (\z -> uncurry f z) x

-- | 
-- An example:
-- let zz = pairPeriods id id s3
-- let ff x = duration.extentPeriod x
-- comparePeriodPairsList ff zz
-- [[6,12,15,19],[12,15,19],[3,7],[7]]
comparePeriodPairsList :: PeriodComparator a -> [PeriodPairs] -> [[a]]
comparePeriodPairsList f x = map (comparePeriodPairs f) x

-- | Returns True if a Period has length 0. False else.
isPoint :: Period -> Bool
isPoint x = (duration x) == 0 

-- | Returns a list of durations from a list of periods.
durations :: [Period] -> [Int]
durations x = map duration x

{-
 Utility functions
-}

-- | Returns an empty list in the case of an empty list.
tailList :: [a] -> [a]
tailList (_:xs)   = xs
tailList []       = []
