{-# LANGUAGE LambdaCase #-}
{- 
Just a module as a scratch base for building new feature builders with IDE
support before moving to a markdown file.
-}

module Templates.Features.BuildScratch(
) where

import Templates.FeatureReqs

{-

   templateArg1 
-> templateArg2 
-> 
Def (
	   F "index" (Index Interval Day)
	-> F "events" [Event Day]
	-> F "age" Natural
)
-}

-- data MyTimes = Day' Integer  | DateTime' Integer Integer

-- g :: MyTimes -> Integer 
-- g (Day' x) = 1
-- g (DateTime' x y) = 5

fAge :: (KnownSymbol age, Ord a) => 
     (Year -> a)
  -> (a -> a -> Integer)
  -> (Index Interval a -> [Event a] -> F age Natural)
fAge f ageComputer index events = 
    viewBirthYears events
  |> map f
  |> map (\x -> ageComputer x (begin index))
  |> headMay
  |> \case
      Just x  -> pure $ fromInteger  x
      Nothing -> makeFeature $ featureDataL $ Other "no birth year found"

fAgeDay :: 
    KnownSymbol age =>
    Index Interval Day
  -> [Event Day]
  -> F age Natural
fAgeDay = fAge (\x -> fromGregorian x 7 15) computeAgeAt

-- fAgeDateTime :: 

buildAge :: 
  ( KnownSymbol indexName
  , KnownSymbol eventsName
  , KnownSymbol ageName
  , Ord a) => 
     (Year -> a)
  -> (a -> a -> Integer)
  -> 
  Def (
     F indexName (Index Interval a)
  -> F eventsName [Event a]
  -> F ageName Natural
  )
buildAge f g = defineA (fAge f g)