module DummyData (
   z0, z1, z2, z4, zs,
   s1, s2, s3,
   index, enrolled
) where 

import Hasklepias.IntervalAlgebra
import Hasklepias.IntervalFilter
import Hasklepias.Features

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

{-
 Define index as the end of the second period after 10
-}
index = makeFeature (intervalToEndPoint . second . (filter (\x -> (end x) > 10)))

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods (+ allowableGap) that are overlapped by the lookbackPeriod are less
than maxGap
-}

enrolled lookbackPeriod allowableGap = 
    all (< allowableGap) . 
    durations . 
    periodGaps . 
    filterOverlappedBy lookbackPeriod . 
    collapsePeriods

{- 
Define analogous of 2-out 1-in
-}