module DummyData (
--   z0, z1, z2, z4, zs,
--   s1, s2, s3,
   e1,
   indexPeriod
   --index, enrolled, twoout, features, baselineFilter
) where 


import Data.List
import Hasklepias.IntervalAlgebra
import Hasklepias.IntervalAlgebra.IntervalFilter
--import Hasklepias.Features
import Hasklepias.Events
import Hasklepias.Context
import Hasklepias.Context.ClaimsDomain


-- | Sequences of events to play with
s1 = map toPeriod [(0, 1), (1, 5), (3, 5), (6, 8), (9, 9), (12, 14), (12, 20)]
s2 = map toPeriod [(1, 3), (1, 7), (3, 5), (6, 8), (7, 11)]
s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]
s4 = map toPeriod [(1, 5), (6, 10), (11, 15), (16, 20)]

c1 = eventContext (Just ["enrollment"])  Nothing Nothing
c2 = eventContext (Just ["duck_struck"]) Nothing Nothing
c3 = eventContext (Just ["gator_crushed"]) Nothing Nothing

-- | e1 is a subject who is:
-- repeatedly struck by a duck
-- enrolled from 1 to 20 a gap of 1 between each 5 day period

e1 = sort $ (zipWith event s1 $ repeat c2) ++ (zipWith event s4 $ repeat c1)

{-
 Define index as the end of the second period after 10
-}
--index :: [Period] -> Period
--index  = endPoint . head . second . (filter (\x -> (end x) > 10))

data Feature a =
    Failure
  | Success { feature :: a , label :: String}

getPeriods :: Events -> [Period]
getPeriods = map (\x -> fst $ getEvent x) 

duckStruck :: Events -> Events
duckStruck = filter (hasConcept "duck_struck")

(!!-)  :: [a] -> Int -> [a]
xs     !!- n | n < 0 =  []
[]     !!- _         =  []
(x:_)  !!- 0         =  [x]
(_:xs) !!- n         =  xs !!- (n-1)


first  x = x !!- 0
second x = x !!- 1
third  x = x !!- 2
fourth x = x !!- 3
fifth  x = x !!- 4

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

{-
 Define index as the end of the first duck struck period after 10
-}

defineFeature :: String -> (Events -> Maybe a) -> Feature a
--defineFeature label f 

indexPeriod :: Events -> Maybe Period
indexPeriod =
  fmap endPoint .
  safeHead . 
  first . 
  filter (\x -> (end x) > 10) . 
  getPeriods . 
  duckStruck 

{-
 Define interval filters
-}

--baselineFilter :: Period -> ([Period] -> [Period])
--baselineFilter x = filterOverlappedBy (expandl 15 x)

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods (+ allowableGap) that are overlapped by the lookbackPeriod are less
than maxGap
-}
{-
enrolled allowableGap indexPoint =
   all (< allowableGap) . 
   durations . 
   periodGaps . 
   baselineFilter indexPoint . 
   collapsePeriods

{- 
Define analogous of 2-out (any two startPoints separated by more than allowableGap)
-}

twoout allowableGap indexPoint  = 
   any (== True) .
   map (\x -> maximum x > allowableGap) . 
   comparePeriodPairsList (\x -> duration.extentPeriod x) . 
  -- get the duration of each pair of periods
  pairPeriods id id . 
  -- form pairs of startpoints
  baselineFilter indexPoint .
  beginPoints

features = (index, enrolled 10, twoout 5)
-}
