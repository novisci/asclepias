module DummyData (
   z0, z1, z2, z4, zs,
   s1, s2, s3,
   index, enrolled, twoout, features, baselineFilter
) where 

import Hasklepias.IntervalAlgebra
import Hasklepias.IntervalFilter
import Hasklepias.Features
import Hasklepias.Events
import Hasklepias.Context
import Hasklepias.Context.ClaimsDomain

-- Some data to play with
z0 = period (-1) (-1)
z1 = period 0 1
z2 = period 5 10
z3 = period 10 11
z4 = period 10 15
zs = [z0, z1, z2, z3, z4]

s1 = map toPeriod [(0, 1), (1, 5), (3, 5), (6, 8), (9, 9), (12, 14), (12, 20)]
s2 = map toPeriod [(1, 3), (1, 7), (3, 5), (6, 8), (7, 11)]
s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]

dx = domain $ Diagnosis Inpatient (Code "W61.62" ICD10) Nothing Nothing
ii = domain $ Insurance "hmo" "ACME Insurance" 

c1 = eventContext (Just ["enrollment"])  ii Nothing
c2 = eventContext (Just ["duck_struck"]) dx Nothing
x1 = event ( period 1 5  ) c1
x2 = event ( period 5 10 ) c1
x3 = event ( period 9 9  ) c2
x4 = event ( period 10 15) c1

z  = [x1, x2, x3, x4]



{-
 Define index as the end of the second period after 10
-}
index :: [Period] -> Period
index  = endPoint . head . second . (filter (\x -> (end x) > 10))

{-
 Define interval filters
-}

baselineFilter :: Period -> ([Period] -> [Period])
baselineFilter x = filterOverlappedBy (expandl 15 x)

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods (+ allowableGap) that are overlapped by the lookbackPeriod are less
than maxGap
-}
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
  startPoints

features = (index, enrolled 10, twoout 5)
