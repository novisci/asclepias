
module MaybeEitherExamples ( ) where

import           Control.Monad
import           Data.Maybe

  {--
     WHAT THIS COVERS

     - Common wrapper types for fallible operations: Maybe and Either
     - Functor, Applicative and Monad typeclasses
     - Writing your own instances of those type classes
     - Application: Secret Santa

     PRE-REQUISITES

     - TypeExamples
     - FunctionalExamples

     --}


  {--
    I GOT A MONAD FOR CHRISTMAS

    My family celebrates Christmas. Some family members seem to think everyone wants a large number of cheap and useless gifts. Nobody else does. But we can't say 'I don't want this garbage' without offending the person. So we made a Secret Santa pool years ago, and it's a decent solution.

    Let's build an app to take a collection of names, apply some rules, and return a collection of Santa-Santee pairings. Actually, we won't build the app here, which would involve reading and writing data and creating an executable. We'll just build the underlying logic, which would go in some ./src directory of the project.

    But first, an introduction to the tools we need.
      --}




{--

 MAYBE AN INTRODUCTION

 First a few things about 'Maybe'.

 A data structure that you can think of as a container with two states: 'full' and 'empty'.
 It can hold any type, and its declaration is very simple
 https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC-Maybe.html#Maybe

 The state of being 'full' of some type a is represented as `Just a`.
 The state of being empty is `Nothing`.
 For those who are familiar with this terminology: It is a sum time.


--}

-- Maybe for fallible operations
-- One of the main use-cases of Maybe is for safely handling fallible
-- operations, in which you do not care how or why an operation failed. In this
-- case, `Just` holds some valid value, and `Nothing` represents an invalid
-- state.

-- Example: Converting a Double to an Integer
-- Double already has `round` and `truncate` methods, which can be used to
-- convert Double to Integer NOTE: If you're familiar with typeclasses, these
-- come from the RealFrac typeclass, of which Double is an instance.

-- a contrained version of round that specifies we want the input to be Double
-- and output to be Integer. The only thing happening here is the type
-- signature, which gives the contraint.
round' :: Double -> Integer
round' = round


-- A safer version of round
-- `round` loses information if its argument is not already a whole number
-- For our application, we might have some need to capture a Double output from
-- some other function and convert it to Integer *only if* the conversion is
-- lossless. Here, we return the result wrapped in a Maybe container, with
-- Nothing representing a failed conversion. Try this in ghci on doubles 1.0 and 1.5.
safeRound :: Double -> Maybe Integer
safeRound x
  | r == 0 = Just i
  | otherwise = Nothing
  where
    (i, r) = properFraction x

-- Why is this 'safe?'
-- For example, we can now pass the output of safeRound to a function that
-- processes Integers if they appear in Just, and leaves Nothing inputs untouched

-- Example One: a dummy example returning the quotient of i by 2
-- quot requires i to be an instance of the Integral typeclass, of integral-like numbers.
-- If the pattern matching here looks weird, see some of the other example files.
quotMaybe :: Maybe Integer -> Maybe Integer
quotMaybe Nothing  = Nothing
quotMaybe (Just i) = Just (i `quot` 2)

-- Example Two: Haskell provides several functions for this kind of operation,
-- in which you apply a function to the Just value or leave Nothing untouched.
-- fmap is one such. Notice (`quot` 2) is now a function of i
quotMaybe' :: Maybe Integer -> Maybe Integer
quotMaybe' = fmap (`quot` 2)

-- Example Three: We have quite a few options for working with collections of Maybe values. See the Data.Maybe and Control.Monad modules in the base package, or the various methods associated with Maybe in the Prelude.

-- justRoundList takes a list of doubles and returns all successful Integer results of safeRound, chucking out all Nothings. See documentation for mapMaybe in Data.Maybe.

justRoundList :: [Double] -> [Integer]
justRoundList = mapMaybe safeRound

-- safeRoundList maps safeRound over a list as well, but in this case it returns Maybe [Integer], returning Nothing if *any* case of safeRound returned Nothing
safeRoundList :: [Double] -> Maybe [Integer]
safeRoundList = mapM safeRound

-- safeRoundSum takes a list of Doubles, attempts to round with safeRound, and
-- adds everything together if all results can be rounded, returning a Just of
-- the result and Nothing if at least one input cannot be rounded without loss.
-- the anonymous function  is maybe a little confusing here: it takes an integer i and a double x and attempts to add i to the result of safeRound x. think of i as the valid sum from the previous iteration.
-- for example, safeRoundSum [1.0, 2.4] can be thought of as executing in the sequence
-- fmap (+ 0) (safeRound 1.0) = Just 1
-- fmap (+ 1) (safeRound 2.4) = Nothing
safeRoundSum :: [Double] -> Maybe Integer
safeRoundSum = foldM (\i x -> fmap (+ i) (safeRound x)) 0

-- Stuff to try in ghci
itsJustThree :: Maybe Integer
itsJustThree = quotMaybe' (safeRound 6.0)

itsNothing :: Maybe Integer
itsNothing = quotMaybe' (safeRound 6.00001)

onlyOnes :: [Integer]
onlyOnes = justRoundList [1.0, 1.5, 2.1, 10.9, 1.0]

myInts :: Maybe [Integer]
myInts = safeRoundList [1.0, 1.5, 2.1, 10.9, 1.0]

aGoodSum :: Maybe Integer
aGoodSum = safeRoundSum [1.0, 2.0]

aBadSum :: Maybe Integer
aBadSum = safeRoundSum [1.0, 2.4]




  {-- SANTA TIME --}

  {--
     First Try:

     Institute a piety rule, in which Santas can't santa each other. Use a list of tuples as your data type.
     --}



{--
  Second Try: Two Santas walk into a Monad...

  TODO

--}


  {--
    Third try: something abstract

TODO
--}

newtype Gift a = Gift (a, a)
type GiftInt = Gift Int


-- define an abstract relation


