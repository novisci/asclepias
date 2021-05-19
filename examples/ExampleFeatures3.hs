{-|
Module      : ExampleFeatures3
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExampleFeatures3(
    exampleFeatures3Spec
) where

import Hasklepias
import ExampleEvents ( exampleEvents4 )
import Test.Hspec


examplePairComparison :: (Intervallic Interval a
              , IntervalSizeable a b) =>
               FeatureDefinition (Interval a) a (Bool, Maybe a)
examplePairComparison = defineFEF Unknown
    ( \i es ->
       es
    |> filterConcur i                   -- filter to concurring with followup interval    
    |> splitByConcepts ["c1"] ["c2"]    -- form a list of pairs where first element
    |> uncurry allPairs                 -- has "c1" events and second has "c2" events    
                                        
    |> filter                           -- filter this list of pairs to cases 
        (\pr -> fst pr `concur`             -- where "c1" event concurs with +/- 3
            expand 3 3 (snd pr) )           -- of any "c2" event 
    |> fmap fst
    |> intervals                        -- get the intervals of any "c1" events
    |> (\x ->
        ( isNotEmpty x                  -- are there any?
        , fmap begin (lastMay x)))      -- if exists, keep the begin of the last "c1" interval
    )


flwup :: Feature (Interval Int)
flwup = featureR $ beginerval 50 0

exampleFeatures3Spec :: Spec
exampleFeatures3Spec = do

    it "examplePairComparison"  $
        applyFEF examplePairComparison flwup exampleEvents4 `shouldBe`
        featureR (True, Just 16)

