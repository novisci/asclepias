module DummyData (
   z0, z1, z2, z4, zs, box, bs,
   s1, s2, s3, b1, b2, b3,
   index, enrolled
) where 

import Hasklepias.IntervalAlgebra
import Hasklepias.Features
-- Some data to play with
z0 = period (-1) (-1)
z1 = period 0 1
z2 = period 5 10
z3 = period 10 11
z4 = period 10 15
zs = [z0, z1, z2, z3, z4]
box x = [x] -- TODO: this seems kludgey as all get out
bs = map box zs

s1 = map toPeriod [(0, 1), (1, 5), (3, 5), (6, 8), (9, 9), (12, 14), (12, 20)]
b1 = map box s1
s2 = map toPeriod [(1, 3), (1, 7), (3, 5), (6, 8), (7, 11)]
b2 = map box s2
s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]
b3 = map box s3

{-
 Define index as the end of the second period after 10
-}
index = makeFeature (intervalToEndPoint . second . (filter (\x -> (end x) > 10)))

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods that are overlapped by  (indexPoint - lookback, indexPoint) are less than 5
-}

enrolled indexPoint lookback gap = all (< gap) . durations . periodGaps .
 filter (\x -> x `overlappedBy` (expandl lookback indexPoint)) . collapsePeriods