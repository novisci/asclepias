{-|
Module      : ExampleFeatures4
Description : Demostrates how to define an outcome monitoring treatment regimes
              over time.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module ExampleFeatures4
  ( exampleFeatures4Spec
  ) where

import           ExampleEvents                  ( exampleEvents4 )
import           Hasklepias
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , pending
                                                , shouldBe
                                                , xcontext
                                                )

{-
  Example Data and utilities to create such
-}

type EventData a b = (b, a, Text)

t1 :: (a, b, c) -> a
t1 (x, _, _) = x
t2 :: (a, b, c) -> b
t2 (_, x, _) = x
t3 :: (a, b, c) -> c
t3 (_, _, x) = x

toEvent
  :: (IntervalSizeable a b, Show a, Integral b) => EventData a b -> Event a
toEvent x = event (beginerval (t1 x) (t2 x))
                  (context (UnimplementedDomain ()) (packConcepts [t3 x]) Nothing)

toEvents
  :: (Show a, IntervalSizeable a b, Integral b) => [EventData a b] -> Events a
toEvents = sort . map toEvent

sapExample1 :: Events Day
sapExample1 = toEvents
  [(1, fromGregorian 2017 1 1, "index"), (1, fromGregorian 2017 3 1, "pcsk")]

sapExample2 :: Events Day
sapExample2 = toEvents
  [ (1, fromGregorian 2017 1 1, "index")
  , (1, fromGregorian 2017 3 1, "wellness")
  ]

p1Events :: Events Int
p1Events = toEvents [(1, 1, "index"), (1, 7 + 15, "pcsk")]

p2Events :: Events Int
p2Events = toEvents [(1, 1, "index"), (1, 7 + 60, "pcsk")]

p3Events :: Events Int
p3Events = toEvents [(1, 1, "index"), (1, 7 + 120, "pcsk")]

p4Events :: Events Int
p4Events = toEvents [(1, 1, "index"), (1, 7 + 240, "pcsk")]

p5Events :: Events Int
p5Events = toEvents [(1, 1, "index")]

{-
  Types used for features
-}

data CensorReason =
  -- The order matters here in that if two censoring events occur on the same
  -- day then the reason for censoring will be chosen based on the following 
  -- ordering.
    DeathCensor -- disambiguating from Death Domain
  | Disenrollment
  | Discontinuation
  | Noncompliance
  | EndOfData
  deriving (Eq, Show, Ord)

instance OccurrenceReason CensorReason

data OutcomeReason =
    Wellness
  | Accident
  -- etc
  deriving (Eq, Show, Ord)

instance OccurrenceReason OutcomeReason

data NegOutcomes b = MkNegOutcome
  { g1 :: CensoredOccurrence CensorReason OutcomeReason b
  , g2 :: CensoredOccurrence CensorReason OutcomeReason b
  , g3 :: CensoredOccurrence CensorReason OutcomeReason b
  , g4 :: CensoredOccurrence CensorReason OutcomeReason b
  , g5 :: CensoredOccurrence CensorReason OutcomeReason b
  }
  deriving Eq

instance (Show b) => Show ( NegOutcomes b ) where
  show (MkNegOutcome x1 x2 x3 x4 x5) =
    "\n g1: "
      ++ show x1
      ++ "\n g2: "
      ++ show x2
      ++ "\n g3: "
      ++ show x3
      ++ "\n g4: "
      ++ show x4
      ++ "\n g5: "
      ++ show x5
      ++ "\n"

data ProtocolStatus a =
    Compliant
  | NonCompliant (EventTime a)
  deriving (Eq, Show)

data Protocols a = MkProtocols
  { noInit  :: ProtocolStatus a
  , init30  :: ProtocolStatus a
  , init90  :: ProtocolStatus a
  , init180 :: ProtocolStatus a
  , init365 :: ProtocolStatus a
  }
  deriving Eq

instance (Show a) => Show (Protocols a) where
  show (MkProtocols x1 x2 x3 x4 x5) =
    "\n "
      ++ show x1
      ++ "\n "
      ++ show x2
      ++ "\n "
      ++ show x3
      ++ "\n "
      ++ show x4
      ++ "\n "
      ++ show x5
      ++ "\n"

{-
  Helper functions
-}

-- | Duration of follow up in days
followupDuration :: Integral b => b
followupDuration = 365

-- | Duration to not observe events after the begin of index.
washoutDuration :: Integral b => b
washoutDuration = 7

-- | Creates an interval *starting 7 days after the index* and 
--   ending 'followupDuration' days later.
makeFollowupInterval
  :: (Integral b, Intervallic i a, IntervalSizeable a b)
  => b
  -> Index i a
  -> Interval a
makeFollowupInterval dur index =
  beginerval dur (add washoutDuration (begin index))

-- | Creates an interval *starting 7 days after the index* and 
--   ending 'followupDuration' days later.
followupInterval
  :: (Integral b, IntervalSizeable a b) => Index Interval a -> Interval a
followupInterval = makeFollowupInterval 365

{-
  Functions for defining the study's exposure protocol(s)
-}

protocol
  :: ( Intervallic i0 a
     , Intervallic i1 a
     , Intervallic i2 a
     , IntervalSizeable a b
     , Filterable container
     )
  => (Index i2 a -> i0 a)
    -- ^ Function that maps an index interval to interval during which protocol is evaluated
  -> (i0 a -> container (i1 a) -> ProtocolStatus b)
    -- ^ Function that maps data to a @ProtocolStatus@.
  -> Index i2 a
  -> container (i1 a)
  -> ProtocolStatus b
protocol g f i dat = f (g i) (filterConcur (g i) dat)

compliantIfNone
  :: ( IntervalSizeable a b
     , Intervallic i0 a
     , Intervallic i1 a
     , Witherable container
     )
  => i0 a
  -> container (i1 a)
  -> ProtocolStatus b
compliantIfNone i x
  | null x = Compliant
  | otherwise = NonCompliant
    (mkEventTime (fmap (`diff` begin i) (end <$> headMay (toList x))))

compliantIfSome
  :: ( IntervalSizeable a b
     , Intervallic i0 a
     , Intervallic i1 a
     , Witherable container
     )
  => i0 a
  -> container (i1 a)
  -> ProtocolStatus b
compliantIfSome i x
  | null x    = NonCompliant (mkEventTime (Just $ diff (end i) (begin i)))
  | otherwise = Compliant

protocolNoInit
  :: ( Integral b
     , IntervalSizeable a b
     , Intervallic i0 a
     , Intervallic i1 a
     , Witherable container
     )
  => Index i0 a
  -> container (i1 a)
  -> ProtocolStatus b
protocolNoInit = protocol (makeFollowupInterval 365) compliantIfNone

protocols
  :: ( Integral b
     , IntervalSizeable a b
     , Intervallic i0 a
     , Intervallic i1 a
     , Witherable container
     )
  => Index i0 a
  -> container (i1 a)
  -> Protocols b
protocols i e = MkProtocols
  (protocol (makeFollowupInterval 365) compliantIfNone i e)
  (protocol (makeFollowupInterval 30) compliantIfSome i e)
  (protocol (makeFollowupInterval 90) compliantIfSome i e)
  (protocol (makeFollowupInterval 180) compliantIfSome i e)
  (protocol (makeFollowupInterval 365) compliantIfSome i e)

-- adminCensor :: (Integral b) => EventTime b -> CensoredOccurrence c o b
-- adminCensor t = MkCensoredOccurrence AdminCensor ( RightCensored t )

compliantOutcome
  :: (Integral b)
  => EventTime b
  -> Occurrence OutcomeReason b
  -> Occurrence CensorReason b
  -> CensoredOccurrence CensorReason OutcomeReason b
compliantOutcome adminTime (MkOccurrence (oreason, otime)) (MkOccurrence (creason, ctime))
  | all (adminTime <) [otime, ctime]
  = adminCensor adminTime
  | all (otime <=) [ctime]
  = MkCensoredOccurrence (O oreason) (Uncensored otime)
  | otherwise
  = MkCensoredOccurrence (C creason) (RightCensored ctime)

nonCompliantOutcome
  :: (Integral b)
  => EventTime b
  -> EventTime b
  -> Occurrence OutcomeReason b
  -> Occurrence CensorReason b
  -> CensoredOccurrence CensorReason OutcomeReason b
nonCompliantOutcome etime adminTime (MkOccurrence (oreason, otime)) (MkOccurrence (creason, ctime))
  | all (adminTime <) [otime, ctime, etime]
  = adminCensor adminTime
  | all (otime <=) [ctime, etime]
  = MkCensoredOccurrence (O oreason) (Uncensored otime)
  | etime <= ctime
  = MkCensoredOccurrence (C Noncompliance) (RightCensored etime)
  | otherwise
  = MkCensoredOccurrence (C creason) (RightCensored ctime)

decideOutcome
  :: (Integral b)
  => EventTime b      -- ^ admin censoring time
  -> ProtocolStatus b -- ^ pcsk
  -> Occurrence OutcomeReason b    -- ^ time of outcome
  -> Occurrence CensorReason b    -- ^ time of censoring (other than noncompliance)
  -> CensoredOccurrence CensorReason OutcomeReason b
decideOutcome adminTime exposure outcomeTime censorTime = case exposure of
  Compliant      -> compliantOutcome adminTime outcomeTime censorTime
  NonCompliant t -> nonCompliantOutcome t adminTime outcomeTime censorTime

{-
   Features needed to evaluate censoring and outcome events
-}
index :: (Ord a) => Def (F "events" (Events a) -> F "index" (Index Interval a))
index = defineA
  (makeConceptsFilter ["index"] .> intervals .> headMay .> \case
    Nothing -> makeFeature $ featureDataL (Other "no index")
    Just x  -> pure $ makeIndex x
  )

flupEvents
  :: (Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Index Interval a)
       -> F "events" (Events a)
       -> F "allFollowupEvents" (Events b)
       )
flupEvents = define
  (\index es -> es |> filterConcur (followupInterval index) |> fmap
    (diffFromBegin (followupInterval index))
  )

{-
   Censoring Events
-}

death
  :: Integral b
  => Def (F "allFollowupEvents" (Events b) -> F "death" (EventTime b))
death = define (mkEventTime . fmap begin . firstConceptOccurrence ["death"])

disenrollment
  :: (Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Index Interval a)
     -- using all events rather than just follow-up events because enrollment
     -- intervals need to be combined first
       -> F "events" (Events a)
       -> F "disenrollment" (EventTime b)
       )
disenrollment = define
  (\i events ->
    events
      |> makeConceptsFilter ["enrollment"]
  -- combine any concurring enrollment intervals
      |> combineIntervals
  -- find gaps between any enrollment intervals (as well as bounds of followup )
      |> gapsWithin (followupInterval i)
  -- get the first gap longer than 30 days (if it exists)
      |> \x ->
           (headMay . filter (\x -> duration x > 30))
             =<< x
  -- Shift endpoints of intervals so that end of follow up is reference point
             |>  fmap (diffFromBegin (followupInterval i))
  -- take the end of this gap as the time of disenrollment
             |>  fmap end
             |>  mkEventTime
  )

-- | A collector feature for all censors (except noncompliance)
censorTime
  :: (Integral b)
  => Def
       (  F "death" (EventTime b)
       -> F "disenrollment" (EventTime b)
  -- etc
       -> F "censortime" (Occurrence CensorReason b)
       )
censorTime = define
  (\dth disrl -> minimum
    [ makeOccurrence DeathCensor   dth
    , makeOccurrence Disenrollment disrl
              -- etc
    ]
  )

{- 
  Exposure Definitions
-}

pcskEvents :: Def (F "events" (Events a) -> F "pcskEvents" (Events a))
pcskEvents = define (makeConceptsFilter ["pcsk"])

pcskProtocols
  :: (Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Index Interval a)
       -> F "pcskEvents" (Events a)
       -> F "pcskProtocols" (Protocols b)
       )
pcskProtocols = define protocols

{-
  Outcome definitions
-}

makeg
  :: (Integral b, IntervalSizeable a b)
  => b
  -> Index Interval a
  -> ProtocolStatus b
  -> Occurrence OutcomeReason b
  -> Occurrence CensorReason b
  -> CensoredOccurrence CensorReason OutcomeReason b
makeg dur i =
  decideOutcome (mkEventTime $ Just $ duration (makeFollowupInterval dur i))

makeNegOutcomes
  :: (Integral b, IntervalSizeable a b)
  => Index Interval a
  -> Protocols b
  -> Occurrence CensorReason b
  -> Occurrence OutcomeReason b
  -> NegOutcomes b
makeNegOutcomes i (MkProtocols p1 p2 p3 p4 p5) c o = MkNegOutcome
  (makeg 365 i p1 o c)
  (makeg 30 i p2 o c)
  (makeg 90 i p3 o c)
  (makeg 180 i p4 o c)
  (makeg 365 i p5 o c)

type OutcomeFeature name a b
  =  F "index" (Index Interval a)
  -> F "allFollowupEvents" (Events b)
  -> F "pcskProtocols" (Protocols b)
  -> F "censortime" (Occurrence CensorReason b)
  -> F name (NegOutcomes b)

makeOutcomeDefinition
  :: (KnownSymbol name, Integral b, IntervalSizeable a b)
  => [Text]
  -> OutcomeReason
  -> Def
       (  F "index" (Index Interval a)
       -> F "allFollowupEvents" (Events b)
       -> F "pcskProtocols" (Protocols b)
       -> F "censortime" (Occurrence CensorReason b)
       -> F name (NegOutcomes b)
       )
makeOutcomeDefinition cpt oreason = define
  (\index events protocols censor ->
    events |> firstConceptOccurrence cpt |> \x ->
      makeOccurrence oreason (mkEventTime (fmap begin x))
        |> makeNegOutcomes index protocols censor
  )

o1 :: (Integral b, IntervalSizeable a b) => Def (OutcomeFeature "wellness" a b)
o1 = makeOutcomeDefinition ["wellness"] Wellness

o2 :: (Integral b, IntervalSizeable a b) => Def (OutcomeFeature "accident" a b)
o2 = makeOutcomeDefinition ["accident"] Accident

{- 
   Tests of protocols
-}

testProtocols
  :: (Integral b, IntervalSizeable a b)
  => [Event a]
  -> Feature "pcskProtocols" (Protocols b)
testProtocols input = eval pcskProtocols idx pcev
 where
  evs  = pure input
  idx  = eval index evs
  pcev = eval pcskEvents evs

p1Protocols :: Feature "pcskProtocols" (Protocols Int)
p1Protocols = pure $ MkProtocols (NonCompliant (mkEventTime (Just 15)))
                                 Compliant
                                 Compliant
                                 Compliant
                                 Compliant

p2Protocols :: Feature "pcskProtocols" (Protocols Int)
p2Protocols = pure $ MkProtocols (NonCompliant (mkEventTime (Just 60)))
                                 (NonCompliant (mkEventTime (Just 30)))
                                 Compliant
                                 Compliant
                                 Compliant

p3Protocols :: Feature "pcskProtocols" (Protocols Int)
p3Protocols = pure $ MkProtocols (NonCompliant (mkEventTime (Just 120)))
                                 (NonCompliant (mkEventTime (Just 30)))
                                 (NonCompliant (mkEventTime (Just 90)))
                                 Compliant
                                 Compliant

p4Protocols :: Feature "pcskProtocols" (Protocols Int)
p4Protocols = pure $ MkProtocols (NonCompliant (mkEventTime (Just 240)))
                                 (NonCompliant (mkEventTime (Just 30)))
                                 (NonCompliant (mkEventTime (Just 90)))
                                 (NonCompliant (mkEventTime (Just 180)))
                                 Compliant

p5Protocols :: Feature "pcskProtocols" (Protocols Int)
p5Protocols = pure $ MkProtocols Compliant
                                 (NonCompliant (mkEventTime (Just 30)))
                                 (NonCompliant (mkEventTime (Just 90)))
                                 (NonCompliant (mkEventTime (Just 180)))
                                 (NonCompliant (mkEventTime (Just 365)))

{- 
   Tests of outcomes
-}

testOutcomes
  :: (Integral b, IntervalSizeable a b)
  => [Event a]
  -> ( Feature "wellness" (NegOutcomes b)
     , Feature "accident" (NegOutcomes b)
     )
testOutcomes input =
  (eval o1 idx flevs prot ctime, eval o2 idx flevs prot ctime)
 where
  evs   = pure input
  idx   = eval index evs
  flevs = eval flupEvents idx evs
  pcev  = eval pcskEvents evs
  dth   = eval death flevs
  disen = eval disenrollment idx evs
  prot  = eval pcskProtocols idx pcev
  ctime = eval censorTime dth disen

p1Outcomes
  :: (Integral b)
  => (Feature "wellness" (NegOutcomes b), Feature "accident" (NegOutcomes b))
p1Outcomes =
  ( pure $ MkNegOutcome
    (MkCensoredOccurrence (C Noncompliance)
                          (RightCensored (mkEventTime (Just 15)))
    )
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 30))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 90))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 180))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 365))))
  , pure $ MkNegOutcome
    (MkCensoredOccurrence (C Noncompliance)
                          (RightCensored (mkEventTime (Just 15)))
    )
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 30))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 90))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 180))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 365))))
  )


p1Outcomes'
  :: (Integral b)
  => (Feature "wellness" (NegOutcomes b), Feature "accident" (NegOutcomes b))
p1Outcomes' =
  ( pure $ MkNegOutcome
    (MkCensoredOccurrence (C Noncompliance)
                          (RightCensored (mkEventTime (Just 15)))
    )
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 30))))
    (MkCensoredOccurrence (O Wellness) (Uncensored (mkEventTime (Just 51))))
    (MkCensoredOccurrence (O Wellness) (Uncensored (mkEventTime (Just 51))))
    (MkCensoredOccurrence (O Wellness) (Uncensored (mkEventTime (Just 51))))
  , pure $ MkNegOutcome
    (MkCensoredOccurrence (C Noncompliance)
                          (RightCensored (mkEventTime (Just 15)))
    )
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 30))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 90))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 180))))
    (MkCensoredOccurrence AdminCensor (RightCensored (mkEventTime (Just 365))))
  )

{-
  Test specs
-}

exampleFeatures4Spec :: Spec
exampleFeatures4Spec = do
  describe "tests of exposure protocols" $ do
    it "p1" $ testProtocols p1Events `shouldBe` p1Protocols
    it "p2" $ testProtocols p2Events `shouldBe` p2Protocols
    it "p3" $ testProtocols p3Events `shouldBe` p3Protocols
    it "p4" $ testProtocols p4Events `shouldBe` p4Protocols
    it "p5" $ testProtocols p5Events `shouldBe` p5Protocols

  describe "tests of outcomes" $ do
    it "p1" $ testOutcomes p1Events `shouldBe` p1Outcomes
    it "p1'"
      $          testOutcomes (sort p1Events <> [toEvent (1, 59, "wellness")])
      `shouldBe` p1Outcomes'

    -- describe "SAP examples" $ 
    --   do
    --     it "sap example 1" pending
        -- testOutcomes sapExample1 `shouldBe` ???
