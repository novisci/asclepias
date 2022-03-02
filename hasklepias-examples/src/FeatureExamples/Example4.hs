{-|
Description : Demostrates how to define an outcome monitoring treatment regimes
              over time.
-}

{-# LANGUAGE DataKinds #-}
module FeatureExamples.Example4
  ( example
  ) where

import           ExampleEvents                  ( exampleEvents4 )
import           Hasklepias

{-
  Example Data and utilities to create such
-}

-- NOTE `Events` was removed from edm package. Using an
-- analogous version here, but for the more general
-- `Event`. Specialized here to have second type variable as Text. Void is a marker for a data type we will never use in this example.
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Void.html#t:Void

-- TODO: what is a better way to go about this, in cases where the Facts field
-- of the Context d c (in Event d c a) is not used? I would like to use Void
-- (from Data.Void), but the constructors require a value. Note Void by design
-- has no constructors.
-- note too that the Facts field replaces the previous Domain type. The latter
-- had an 'UnimplementedDomain' variant, but i don't immediately see such a
-- thing for facts. We might wish to have something like that here, to avoid
-- boilerplate like that of AlternativeFacts, in which we have to derive some
-- instances to satisfy all the constraints in Eventable d c a, and the
-- HasConcept constraint --- the latter of which i think needs a rethinking
-- anyway.

data AlternativeFacts = AlternativeFacts
  deriving (Eq, Show, Generic)


-- Defined separately in ExampleFeatures3
type MyEvent a = Event AlternativeFacts Text a
type Events a = [MyEvent a]

-- NOTE this has nothing to do with the event data model. it's a container for
-- event *data*
type EventData a b = (b, a, Text)

t1 :: (a, b, c) -> a
t1 (x, _, _) = x
t2 :: (a, b, c) -> b
t2 (_, x, _) = x
t3 :: (a, b, c) -> c
t3 (_, _, x) = x

-- NOTE needed to add the Typeable constraint to a bunch of these to satisfy
-- the constraints in event after edm theory refactor. see EventDataTheory.Core
toEvent
  :: (Typeable a, IntervalSizeable a b, Show a, Integral b)
  => EventData a b
  -> MyEvent a
toEvent x = event (beginerval (t1 x) (t2 x))
  -- NOTE context constructor switched argument order
                  (context (packConcepts [t3 x]) AlternativeFacts Nothing)

toEvents
  :: (Typeable a, Show a, IntervalSizeable a b, Integral b)
  => [EventData a b]
  -> Events a
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
    DeathCensor
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
  -> i a
  -> Interval a
makeFollowupInterval dur index =
  beginerval dur (add washoutDuration (begin index))

-- | Creates an interval *starting 7 days after the index* and 
--   ending 'followupDuration' days later.
followupInterval
  :: (Integral b, IntervalSizeable a b) => Interval a -> Interval a
followupInterval = makeFollowupInterval 365

-- TODO i changed this to accommodate some constraints in diffFromBegin, used
-- in flupEvents. see note there. this workaround is dumb and should be
-- addressed with changes upstream, probably. In particular, look at the
-- awkward dance of a and b types to satisfy the signature of diffFromBegin.
-- See docs on diffFromBegin for what it does
eventDiffFromBegin
  :: (Typeable b, Ord b, Show b, IntervalSizeable a b, Integral b)
  => Interval a
  -> MyEvent a
  -> MyEvent b
eventDiffFromBegin i e = event i' (getContext e)
  where i' = diffFromBegin i (getInterval e)

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
  => (i2 a -> i0 a)
    -- ^ Function that maps an index interval to interval during which protocol is evaluated
  -> (i0 a -> container (i1 a) -> ProtocolStatus b)
    -- ^ Function that maps data to a @ProtocolStatus@.
  -> i2 a
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
  => i0 a
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
  => i0 a
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
index :: (Ord a) => Def (F "events" (Events a) -> F "index" (Interval a))
index = defineA
  -- NOTE makeConceptsFilter was removed in the event-data-theory work. 
  -- replacing with an equivalent.
  -- NOTE replaced 'intervals' function from IntervalAlgebra with the local
  -- intervals'. the former acts on f (PairedIntervals) only with Functor f,
  -- e.g. [PairedInterval], and the latter acts on any f a is Intervallic, e.g.
  -- [Event d c a].

  -- Note also the type annotation on "index". OverloadedStrings doesn't infer
  -- the correct type here for use in HasConcept instance and says there is no
  -- instance where the second type param is String. that makes sense because
  -- you might in principle have different instances for HasConcept a String
  -- and HasConcept a Text -- which of course would be dumb.
  (  filterEvents (Predicate (`hasConcept` ("index" :: Text)))
  .> intervals'
  .> headMay
  .> \case
       Nothing -> makeFeature $ featureDataL (Other "no index")
       Just x  -> pure x
  )
  where intervals' = fmap getInterval

-- NOTE the additional constraints exist because of those on the new Event. See
-- Eventable. 
flupEvents
  :: (Show b, Typeable b, Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Interval a)
       -> F "events" (Events a)
       -> F "allFollowupEvents" (Events b)
       )
flupEvents = define
  (\index es -> es |> filterConcur (followupInterval index) |> fmap
    -- NOTE this is the code requiring me to change followupInterval.
    -- diffFromBegin has a Functor constraint, and Event d c is not a Functor
    (eventDiffFromBegin (followupInterval index))
  )

{-
   Censoring Events
-}

-- NOTE `firstConceptOccurrence` was axed from hasklepias-core FeatureEvents,
-- placed in event-data-theory and renamed.
death
  :: Integral b
  => Def (F "allFollowupEvents" (Events b) -> F "death" (EventTime b))
death = define (mkEventTime . fmap begin . firstOccurrenceOfConcept ["death"])

disenrollment
  :: (Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Interval a)
     -- using all events rather than just follow-up events because enrollment
     -- intervals need to be combined first
       -> F "events" (Events a)
       -> F "disenrollment" (EventTime b)
       )
disenrollment = define
  (\i events ->
    events
      |> filterEvents (Predicate (`hasConcept` ("enrollment" :: Text)))
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
  (\dth disrl -> fromMaybe
    (makeOccurrence DeathCensor dth)
    -- TODO: the default should be something else just putting this value as an
    --       example
    (minimumMay
      [ makeOccurrence DeathCensor   dth
      , makeOccurrence Disenrollment disrl
              -- etc
      ]
    )
  )

{- 
  Exposure Definitions
-}

pcskEvents :: Def (F "events" (Events a) -> F "pcskEvents" (Events a))
pcskEvents = define $ filterEvents (Predicate (`hasConcept` ("pcsk" :: Text)))

pcskProtocols
  :: (Integral b, IntervalSizeable a b)
  => Def
       (  F "index" (Interval a)
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
  -> Interval a
  -> ProtocolStatus b
  -> Occurrence OutcomeReason b
  -> Occurrence CensorReason b
  -> CensoredOccurrence CensorReason OutcomeReason b
makeg dur i =
  decideOutcome (mkEventTime $ Just $ duration (makeFollowupInterval dur i))

makeNegOutcomes
  :: (Integral b, IntervalSizeable a b)
  => Interval a
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
  =  F "index" (Interval a)
  -> F "allFollowupEvents" (Events b)
  -> F "pcskProtocols" (Protocols b)
  -> F "censortime" (Occurrence CensorReason b)
  -> F name (NegOutcomes b)

makeOutcomeDefinition
  :: (KnownSymbol name, Integral b, IntervalSizeable a b)
  => [Text]
  -> OutcomeReason
  -> Def
       (  F "index" (Interval a)
       -> F "allFollowupEvents" (Events b)
       -> F "pcskProtocols" (Protocols b)
       -> F "censortime" (Occurrence CensorReason b)
       -> F name (NegOutcomes b)
       )
makeOutcomeDefinition cpt oreason = define
  (\index events protocols censor ->
    events |> firstOccurrenceOfConcept cpt |> \x ->
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
  => [MyEvent a]
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
  :: (Show b, Typeable b, Integral b, IntervalSizeable a b)
  => [MyEvent a]
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

example :: TestTree
example = testGroup
  "TODO"
  [ testCase "TODO" $ testProtocols p1Events @?= p1Protocols
  , testCase "TODO" $ testProtocols p2Events @?= p2Protocols
  , testCase "TODO" $ testProtocols p3Events @?= p3Protocols
  , testCase "TODO" $ testProtocols p4Events @?= p4Protocols
  , testCase "TODO" $ testProtocols p5Events @?= p5Protocols
  , testCase "TODO" $ testOutcomes p1Events @?= p1Outcomes
  , testCase "TODO"
  $   testOutcomes (sort p1Events <> [toEvent (1, 59, "wellness")])
  @?= p1Outcomes'
  ]

exampleFeatures4Spec :: TestTree
exampleFeatures4Spec = testGroup "tests of exposure protocols"
  [ testCase "p1" $ testProtocols p1Events @?= p1Protocols
  , testCase "p2" $ testProtocols p2Events @?= p2Protocols
  , testCase "p3" $ testProtocols p3Events @?= p3Protocols
  , testCase "p4" $ testProtocols p4Events @?= p4Protocols
  , testCase "p5" $ testProtocols p5Events @?= p5Protocols
  , testCase "p1" $ testOutcomes p1Events @?= p1Outcomes
  , testCase "p1" $ testOutcomes (sort p1Events <> [toEvent (1, 59, "wellness")]) @?= p1Outcomes
  ]
