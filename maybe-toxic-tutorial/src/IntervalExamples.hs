-- NOTE before you can use this module in ghci you must run
-- cabal install interval-algebra --lib
-- this places interval-algebra in the ghc package database
-- only packages in the database will be recognized as importable by ghci
-- see the list of existing packages by running in the terminal ghc-pkg list

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalExamples ( Meeting (..)
                        , Hour (..)
                        , MeetingData
                        , Natural
                        , naturalFromInteger
                        , naturalToInteger
                        , module IntervalAlgebra
                        ) where

import           Data.Either     (fromLeft, fromRight)
import           GHC.Natural     (Natural, naturalFromInteger, naturalToInteger)
import           IntervalAlgebra

  {-
    BASICS

    An Interval in IntervalAlgebra is a data type (like Int or String) endowed
    with three essential pieces of information: A begin, an end and a duration. The main benefit of the Interval type is that it ensures begin, end and duration are defined in a consistent set of units, units which are again represented by types. With this, IntervalAlgebra can also define relations between two intervals of the same type. 

    For example, below we will look at Intervals whose begin, end and duration are all of the Int type. 

    -}

-- CREATING INTERVALS

-- TODO make notes
beginerDefault :: Interval Int
beginerDefault = beginerval 0 0

enderDefault :: Interval Int
enderDefault = enderval 0 0

-- TODO
startFromEnd :: Interval Int
startFromEnd = enderval 2 2

-- TODO
-- a function that builds an interval of smallest possible length starting from
-- a given value. 
unitInterval :: Int -> Interval Int
unitInterval = beginerval 0

-- begin, end, duration are utilities that return these fundamental pieces of
-- information about an interval. 'diff' is a difference function giving the
-- difference between the points provided. For intervals with Int endpoint
-- types, diff y x = y - x.
gospel :: Interval Int -> Bool
gospel x = duration x == diff (end x) (begin x)

-- This is how we'd define a generic version of gospel, using the fundamental
-- typeclass IntervalSizeable. More on this later. Notice that we didn't have
-- to write the definition any differently for the generic case! That's because
-- the functions diff, end and begin are all guaranteed to exist for inputs
-- that are instances of this typeclass.
genericGospel :: (IntervalSizeable a b) => Interval a -> Bool
genericGospel x = duration x == diff (end x) (begin x)

-- CREATING INTERVALS WHEN ERRORS ARE POSSIBLE
-- The Either type is useful for error handling. Either a b has two possible
-- values: Right a (containing a type a) or Left b (containing the 'error' of
-- type b). Associating Left with errors is just convention. This allows us to
-- have functions that might fail. We can then handle the failures or succeses
-- in different ways, as in the nutinRight function below.

-- TODO notes on ParseInterval
-- NOTE this is not the same as beginerval 0 0 since nutin produces an interval with zero length
nutin :: Either ParseErrorInterval (Interval Int)
nutin = parseInterval 0 0

-- Either types often are then 'unwrapped' as a form of error-handling. If the
-- result is Right (Interval Int) then we get the interval out. Othewise, it
-- will be Left ParseErrorInterval and we will return the default.
nutinRight :: Either ParseErrorInterval (Interval Int) -> Interval Int
nutinRight (Right x) = x
nutinRight _         = beginerDefault


-- MODIFYING INTERVALS
-- Two typeclasses largely determine how we can create and
-- manipulate intervals: IntervalSizeable and Intervallic.
-- Now is a good time to check the interval-algebra docs to
-- familiarize yourself with those classes and their
-- methods.
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/IntervalAlgebra-Core.html#t:Intervallic

-- There is really only one way to modify an interval: adjust the left endpoint
-- or the right one. And you can only expand it, you can't make it shorter.

-- expand to the right
longerDefault :: Interval Int
longerDefault = expandr 2 beginerDefault 

-- if the first argument is less than the 'moment' value
-- defined in the appropriate IntervalSizeable instance,
-- then the interval is unchanged. The moment value for
-- IntervalSizeable Int Int is 1, so this operation doesn't
-- change anything.
beginerDefault' = expandr 0 beginerDefault

-- (-1) is still less than 1 so this also doesn't change the interval
longerDefault' = expandr (-1) longerDefault





-- COMPARING INTERVALS
-- many more operations for comparing intervals than these examples show
-- 13 ways two intervals can relate
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/IntervalAlgebra-Core.html#g:5

relDefaults :: IntervalRelation
relDefaults = relate beginerDefault enderDefault

-- predicates for checking relations
-- only one relation can hold for each ordered pair of intervals x y
-- e.g. from docs
-- x: |----|
-- y:      |---|
-- relate y x == MetBy
-- relate x y == Meets
-- relate x y /= Before
-- NOTE: we're using the 'infix' versions of these functions as indicated by
-- the backticks, meaning we do x `metBy` y instead of metBy x y
alwaysFalse :: Bool
alwaysFalse = (enderDefault `metBy` beginerDefault) && (enderDefault `before` beginerDefault)

-- create a predicate function that is the union (logical 'or') of two predicate functions. Note the type signature and compare to that of the operator (<|>) in docs. In particular the ComparativePredicateOf2 type just wraps functions that take two possibly different types of intervals and return a Bool

metOrBefore :: (Intervallic i0 a, Intervallic i1 a) => ComparativePredicateOf2 (i0 a) (i1 a)
metOrBefore = metBy <|> meets <|> before

-- or in a fold. see the wiki for some nice examples.  similar to Reduce in R
-- https://wiki.haskell.org/Foldr_Foldl_Foldl'
metOrBefore' :: (Intervallic i0 a, Intervallic i1 a) => ComparativePredicateOf2 (i0 a) (i1 a)
metOrBefore' = foldr (<|>)  metBy [meets, before]

-- actually, this is as alternate way to define a version of the
-- interval-algebra function unionPredicates, with a default of False if we
-- provide an empty list of predicates. 

-- see the source code for unionPredicates for how it is actually defined
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/src/IntervalAlgebra.Core.html#unionPredicates
-- Look at the type signature of unionPredicates to see that function is more general than the version defined here, which includes some typeclass constraints necessary for using (<|>) 

-- here (\x y -> False) is an anonymous function that always returns False.
-- The constant False alone there would not work because it doesn't fit the
-- type signature needed for (<|>)

unionPredicates' :: (Intervallic i0 a, Intervallic i1 a) => [ComparativePredicateOf2 (i0 a) (i1 a)] -> ComparativePredicateOf2 (i0 a) (i1 a)
unionPredicates' = foldr (<|>) (\x y -> False)

anotherMetOrBefore :: (Intervallic i0 a, Intervallic i1 a) => ComparativePredicateOf2 (i0 a) (i1 a)
anotherMetOrBefore = unionPredicates' [metBy, meets, before]


-- True
-- beginerDefault is (0, 1)
-- enderDefault is (-1, 0)
-- beginerDefault is metBy enderDefault
defaultsMetOrBefore :: Bool
defaultsMetOrBefore = metOrBefore beginerDefault enderDefault

-- Also True, since we consider metBy OR meets
defaultsMetOrBefore' :: Bool
defaultsMetOrBefore' = metOrBefore enderDefault beginerDefault

-- False
-- since relations are disjoint: startFromEnd is (0, 2) and has relation
-- StartedBy relative to beginerDefault
startMetOrBefore :: Bool
startMetOrBefore = metOrBefore startFromEnd beginerDefault


-- MORE UTILITIES
-- and there are still many more
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/IntervalAlgebra-IntervalUtilities.html

-- these first to are not from IntervalAlgebra but will help with examples

-- build an *infinite* list of intervals of unit duration by mapping over end points
beginerList :: [Interval Int]
beginerList = map (beginerval (0::Int)) [0..]

-- make beginerList finite by taking the first N elements
beginerListN :: Int -> [Interval Int]
beginerListN n = take n beginerList


-- a very dumb way to create an interval with duration n 
-- combineIntervals does what it says: combined intervals that meet or share support
-- lengthNInterval 3
-- [(0, 3)]
lengthNInterval :: Int -> [Interval Int]
lengthNInterval = combineIntervals . beginerListN

-- more interesting is what happens when you have a mix of overlapping and disjoint intervals
-- guess and check the output
choppyIntervals :: [Interval Int]
choppyIntervals = combineIntervals [beginerval 1 0, beginerval 2 0, beginerval 2 1, beginerval 2 4, beginerval 1 5]

-- there are several filtering operations, which you can think of as comparing a list of intervals and filtering those that satisfy some relation to a reference interval
-- for example, here we filter beginerList to those such that (10, 11) is *after* a given interval in the list (ie filter intervals before (10, 11)),  where 'after' is the IntervalAlgebra function
tenIsAfter :: [Interval Int]
tenIsAfter = filterAfter (beginerval 1 10) (beginerListN 20)

-- NOTE applying the above to beginerList rather than a finite list crashes the program by trying to evaluate an infinite list. is there not a way for it to terminate, since the list is ordered?


  {- MEETINGS: AN EXTENDED EXAMPLE

      Here we'll mess with a Meeting data type, which represents a single meeting (like those on your calendar) in one-hour chunks.
      The Meeting will be defined in terms of Hour chunks, another type that is a Natural number type with functions to constrain it to be between 0 and 24.
  Skip ahead to see how Meeting and Hour are defined as instances of the main typeclasses in interval-algebra, Intervallic and IntervalSizeable.
-}


-- CREATING A MEETING
-- beginerval, enderval rely on the IntervalSizeable typeclass
-- in this case we use the instance IntervalSizeable Hour Integer

shortestMeeting :: Meeting Hour
shortestMeeting = Meeting (beginerval 0 (Hour 9))

-- note that duration is to a minimum of moment, defined in the
-- IntervalSizeable instance
shortestMeeting' :: Meeting Hour
shortestMeeting' = Meeting (beginerval 1 (Hour 9))

midnightMeeting :: Meeting Hour
midnightMeeting = Meeting (beginerval 0 (Hour 0))

allDayMeeting :: Meeting Hour
allDayMeeting = Meeting (beginerval 24 (Hour 0))

-- Though beginerval says it should last 23 hours, we have baked  the constraint that Hour must be a Natural number within [0, 24] into the add method of the IntervalSizeable Hour Integer instance.
typoTime :: Meeting Hour
typoTime = Meeting (beginerval 23 (Hour 8))

-- impossibleMeet might not give what you expect!
-- equals Meeting (Hour 0, Hour 1)
-- look at the source for beginerval and compare with the definition of add in the IntervalSizeable Hour Integer instance below
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/src/IntervalAlgebra.Core.html#beginerval

-- when all is done, we are silently coercing the inputs such that
-- i) the meeting start time cannot be less than Hour 0 (so -1 gets converted to 0)
-- ii) the minimum meeting length is 1, so the end time is Hour 1
impossibleMeet :: Meeting Hour
impossibleMeet = Meeting (beginerval (-1) (Hour 0))


-- A TYPE-ING ERROR
-- Your IDE should catch this mistake. The Hour constructor takes only Natural
-- numbers. This will generate a compiler warning but will still compile. If you try to do anything with badStart, it would then throw a run-time exception.

-- To upgrade this particular warning to a compile-time error, meaning this
-- mistake never will make it into the program when run, use the ghc flag
-- -Werror=overflowed-literals. You can also use this in ghci, where instead of
-- typying ghci in the terminal you would do
-- ghci -Werror=overflowed-literals

-- To upgrade all warnings to compile-time errors, use the -Werror flag
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html

badStart :: Meeting Hour
badStart = Meeting (beginerval 3 (Hour (-2)))


-- MODIFYING A MEETING
-- Since meeting is an instance of the typeclass Intervallic, and Hour is
-- IntervalSizeable, we get access to all of the methods in interval-algebra
-- for modifying intervals. See below for how those instances are defined in
-- this case.

-- extend shortestMeeting to the right by 1 unit
meetingRunsOver :: Meeting Hour
meetingRunsOver = expandr 1 shortestMeeting

-- 1 happens to be the 'moment' for Hour, meaning the smallest unit increment for our intervals. This is given in the IntervalSizeable typeclass definition.
-- instead of specifying 1 literally, we could make this code easier to
-- refactor if we change moment later, by actually extracting the moment here
-- and expanding by that one unit. You can test this for yourself by manually
-- changing the moment definition to 2 from 1 in the IntervalSizeable Hour
-- Integer instance
meetingRunsOver' :: Meeting Hour
meetingRunsOver' = expandr (moment' shortestMeeting) shortestMeeting


-- PREDICATES FOR SCHEDULING LOGIC
-- we can use interval-algebra to do some basic scheduling tasks. Here's we'll use the predicate functions and interval relations to check for scheduling conflicts.

-- Does my Meeting conflict with any others?
-- this function takes a Meeting m and checks whether it concurs (shares any
-- overlap with) any meeting in a list of meetings, using the interval-algebra
-- predicate concur

-- Note the functional syntax here:
-- i) the second argument, [Meeting Hour] a list of Meeting Hour, is not
-- specified on the left-hand side because for any f taking arguments x y the
-- statement f x is itself a function in y

-- ii) similarly (concur m) is a function that will check whether some interval concurs with m, and any (concur m) will check whether any in a list of intervals concurs with m

-- for a simpler example, try the following in ghci: any (== 1) [0, 1, 4]
conflictsWithAny :: Meeting Hour -> [Meeting Hour] -> Bool
conflictsWithAny m = any (concur m)

-- A little functional programming example
-- the . is an operator on functions. see more in the functional programming
-- basics tutorial (TODO).
-- f . g is the function defined by f(g(x)) for any x
conflictsWithAny' :: Meeting Hour -> [Meeting Hour] -> Bool
conflictsWithAny' = any . concur

-- you also might see the $ operator, similar to . but subtly different
-- the difference is in the type-ing. whereas . is an operator on *functions* $ operates on values.
-- so f $ g has no meaning. f $ g x is the same (in math notation) as f(g(x)) for any x
-- $ is just a way to remove () really, for a debatable gain in readability
-- compare the type of . to that of $ by typing the following into ghci
-- :t (.)
-- :t ($)
-- see also the wiki
-- https://wiki.haskell.org/Function_composition
-- https://wiki.haskell.org/$

-- finally, to hammer it home, try removing the m in the statement below.
-- you'll get a type error.
conflictsWithAny'' :: Meeting Hour -> [Meeting Hour] -> Bool
conflictsWithAny'' m = any $ concur m

-- is my Meeting the first one of the day? 
-- its OK if it 'meets' another interval, meaning its end time matches the
-- start time, which is not true of the Before interval relation. to capture
-- that, we use unionPredicates' first
isFirst :: Meeting Hour -> [Meeting Hour] -> Bool
isFirst m = all (unionPredicates' [before, meets] m)



  {- INTERVALS EMBEDDED IN OTHER TYPES

        The Intervallic typeclass allows us to define how we get and set intervals for objects that themselves might not be intervals but which nonetheless have an interval component. 

        In this case we create a MeetingData type, which is just a meeting and some metadata.

        This also lets us explore Record types in Haskell, including their constructors and some pattern matching. See the definition of MeetingData below for how a record type is defined.

        For reference, the MeetingData Hour type, a record type, is defined as

        MeetingData { mtgInterval :: Meeting Hour
                    , mtgName     :: String
                    , mtgInvited  :: [String]
                    }
      -}

-- for convenience, let's write a creator function like beginerval but that
-- returns a Meeting. This is *not* the most generic we can make it but follows
-- the examples above in using only Meeting Hour rather than the generic
-- Meeting c defined below.
beginerMeet :: Integer -> Hour -> Meeting Hour
beginerMeet i h = Meeting (beginerval i h)

-- a default meeting we can modify
meetingDefault :: MeetingData Hour
meetingDefault = MeetingData { mtgInterval = beginerMeet 0 (Hour 0), mtgName = "", mtgInvited = [] }

-- modify meetingData to contain a one-hour meeting starting at Hour 9
-- and to change its name to Standup

-- since MeetingData is an Intervallic typeclass instance, we get the getInterval and setInterval methods.
morningStandup :: MeetingData Hour
morningStandup = setInterval m (beginerval 1 (Hour 9))
  -- an example of field update in record type in Haskell
  where m = meetingDefault { mtgName = "Standup" }

-- a function to add attendees
-- note the record update syntax on the right-hand side of = and the pattern
-- matching in the where statement. you only need to specify the fields on
-- which you want to match
addInvited :: MeetingData Hour -> [String] -> MeetingData Hour
addInvited m invs = m { mtgInvited = invs ++ old }
  where MeetingData { mtgInvited = old } = m



  {- TYPE CREATION CODE
      This is where we create the Meeting, Hour and MeetingData types, along with specifications for the appropriate instances, such as Intervallic.
      -}

  {-
      MEETING and HOUR

      Meeting is an Interval c, where c is some type giving units for our meeting intervals. This is not a calendar: Meeting has no concept of date, so think of this as meetings happening on a single day. This example will define a Hour type to use there, instead of a general c. Hour is a data type that implements IntervalSizeable, which will allow us to use existing methods from interval-algebra for modifying Meeting Hour as an interval. Note this is not the same Hour type as in the typulations demo, which was an integer circle.
      -}

-- DEFINE HOUR
-- NOTE: if you were actually writing a Meeting data type, you'd use the
-- UTCTime type instead of Hour. UTCTime NormalDiffTime already represent an instance
-- of IntervalSizeable. This example is merely for exposition. See the accompanying exercises.

-- Notice that this does not define Hour as an instance of Num. You *could* do
-- it, but it would violate the understanding given in the docs for Num in the
-- Prelude
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:7
-- For example, the negate operation and related arithmetic makes no sense
-- because Hour is constrained to be positive. Therefore, Hour has no additive
-- inverse. Looking at the IntervalSizeable typeclass signature in the docs
-- shows thankfully we don't actually need Hour itself to be a Num instance.
-- IntervalSizeable is important because it is the typeclass constrained used
-- in fundamental Interval manipulation and creation, such as beginerval.


-- places input into the domain for Hour
-- written in this way so as to accomodate Integral, Int as well as Natural
-- typeclass constraints specified should match the methods used, eg. fromIntegral
domHour :: (Ord a, Num a, Integral a) => a -> Natural
domHour x = fromIntegral (min (max x 0) 24)

-- Define Hour and its instances
newtype Hour
  = Hour Natural
  deriving (Eq, Ord, Show)

instance IntervalSizeable Hour Integer where
  moment = 1 :: Integer

  add c (Hour y) = Hour (domHour (c + naturalToInteger y))

  diff (Hour x) (Hour y) = naturalToInteger x - naturalToInteger y


-- DEFINE MEETING
-- notice the unit type for our meeting is left generic. it can be *any* type.
-- Here we only use Hour. In exercises, we'll want to restrict the types that
-- meeting units can be using typeclasses, and we will consider units other
-- than Hour.

newtype Meeting c
  = Meeting (Interval c)
  deriving (Show)

-- NOTE on the right-hand side we use the getInterval/setInterval methods
-- as defined on Interval a, so there is no recursive behavior here
instance (Ord a) => Intervallic Meeting a where
  getInterval (Meeting x) = getInterval x

  setInterval (Meeting x) y = Meeting (setInterval x y)


  {-
      MeetingData TYPE

      A Meeting with some metadata. Shows how you can embed intervals into larger data structures.
      -}

data MeetingData c
  = MeetingData
      { mtgInterval :: Meeting c
      , mtgName     :: String
      , mtgInvited  :: [String]
      }
  deriving (Show)

-- using getInterval and setInterval from Meeting Intervallic instance
instance (Ord a) => Intervallic MeetingData a where
  getInterval MeetingData { mtgInterval = x } = getInterval x

  setInterval md y = md {mtgInterval = setInterval x y}
    where MeetingData {mtgInterval = x} = md
