module Hasklepias.IntervalAlgebra(
  Period,
  period,
  point,
  isPoint,
  toPeriod,
  startPoint,
  endPoint,
  endPoints,
  start,
  end,
  expandl,
  expandr,
  durations,
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
  duration
) where

type Comparator a = (a -> a -> Bool)

class Periodic a where
    meets, metBy             :: Comparator a
    before, after            :: Comparator a
    overlaps, overlappedBy   :: Comparator a
    moverlaps, moverlappedBy :: Comparator a
    starts, startedBy        :: Comparator a
    ends, endedBy            :: Comparator a
    during, contains         :: Comparator a
    disjoint                 :: Comparator a
    duration                 :: a -> Int

    metBy         = flip meets
    after         = flip before
    overlappedBy  = flip overlaps
    moverlappedBy = flip moverlaps
    startedBy     = flip starts
    endedBy       = flip ends
    contains      = flip during
    disjoint x y  = (before x y) || (after x y)

data Period = 
    Point    { start :: Int, end :: Int}
  | Moment   { start :: Int, end :: Int}
  | Interval { start :: Int, end :: Int}
  deriving (Eq, Read)

type BoxPeriod = [Period]

{- 
These functions assume x <= y
TODO: formalize this notion
 -}

instance Periodic Period where
  meets    x y  = (start y) == (end x)  
  before   x y  = (end x)   < (start y) 
  starts   x y  = (start x) == (start y)
  ends     x y  = (end x)   == (end y)
  during   x y  = (overlaps x y) && (end x) <= (end y)
  overlaps x y  = (start y) < (end x)
  moverlaps x y = meets x y || overlaps x y
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

expandr :: Int -> Period -> Period
expandr i p = period (start p) (end p + i)

startPoint :: Period -> Period
startPoint x = point (start x)

endPoint :: Period -> Period
endPoint x = point (end x)

endPoints :: Period -> BoxPeriod
endPoints x
    | isPoint x = [x]
    | otherwise = [startPoint x, endPoint x]

durations :: BoxPeriod -> [Int]
durations x = map duration x


unBox (x:_) = x 

(<<>>) :: BoxPeriod -> BoxPeriod -> BoxPeriod
(<<>>) xl yl
   | null xl   = yl
   | null yl   = xl
   | otherwise = init xl ++ [period (start x) (end y)] ++ tailList yl
   where x = unBox ( lastList xl )
         y = unBox ( headList yl )

(<++>) :: BoxPeriod -> BoxPeriod -> BoxPeriod
(<++>) xl yl
   | null xl         = yl
   | null yl         = xl
   | x `moverlaps` y = xl <<>> yl
   | x `before` y    = xl ++ yl
   where x = unBox ( lastList xl )
         y = unBox ( headList yl )


(<-->) :: BoxPeriod -> BoxPeriod -> BoxPeriod
(<-->) xl yl
   | null xl         = endPoints y
   | null yl         = endPoints x
   | x `moverlaps` y = init xl ++ init (endPoints x) ++ tail (endPoints y) ++ tailList yl
   | x `before`    y = init xl ++ (endPoints x) <<>> (endPoints y) ++ tailList yl
   where x = unBox ( lastList xl )
         y = unBox ( headList yl )


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

collapsePeriods :: BoxPeriod -> BoxPeriod
collapsePeriods x = foldr (<++>) [] (map (\z -> [z]) x)

periodGaps :: BoxPeriod -> BoxPeriod
periodGaps x = foldr (<-->) [] (map (\z -> [z]) x)

headList :: [a] -> [a]
headList (x:_) = [x]
headList []     = []

tailList :: [a] -> [a]
tailList (_:xs)   = xs
tailList []       = []

lastList :: [a] -> [a]
lastList [x]    = [x]
lastList (_:xs) = lastList xs
lastList []     = []
