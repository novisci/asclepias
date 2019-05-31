module Hasklepias.Features (
   makeFeature,
   intervalToEndPoint,
   Feature, 
   first, 
   second,
   third,
   fourth,
   fifth
) where

import Hasklepias.IntervalAlgebra

data Feature a = Feature (Bool, Maybe a)
   deriving (Show)



makeFeature :: ([a] -> Maybe a) -> [a] -> Feature a
makeFeature f [] = Feature (False, Nothing)
makeFeature f x  = Feature (not (null (f x)), f x)

intervalToEndPoint :: [Period] -> Maybe Period
intervalToEndPoint [] = Nothing
intervalToEndPoint x  = Just ( endPoint (head x))


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

