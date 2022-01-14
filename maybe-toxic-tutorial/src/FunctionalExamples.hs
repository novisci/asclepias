-- IGNORE: i have explicitly turned off certain compiler warnings.
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

  {--
     WHAT YOU WILL LEARN

     -- functions in Haskell: 'pure', 'lambda calculus' and 'currying'
     -- functions as types too
     -- composition (. operator) and application ($ operator)
     -- anonymous functions ('lambdas')
     -- if-then-else, guards
     -- where and let for local definitions
     -- 'functional' style in list operations: maps, folds, filters, list
        comprehensions
     -- example of these applied to creating the set of all paths on a list of
        elements.

     PRE-REQUISITES

     Knowledge of basic types and type signatures.

     It's probably best to start with the TypeExamples module, though you can
     probably start here if you've done some work in an intro text like Learn
     You a Haskell ... or Haskell from First Principles.

     --}

module FunctionalExamples ( ) where

import           Data.List (intersect, nub, permutations)

   {- FUNCTIONS IN HASKELL
       SIDE-BAR on SIDE-EFFECTS
       -}

-- Functions in general have two properties (defining 'pure' functions):
-- Same inputs produce the same outputs.
-- No side-effects.

-- Explaining 'no side-effects'
-- In many languages, you can modify objects 'in-place'
-- For example, in C++ you could write

   {-
#include <iostream>

using namespace std;

int counter = 0;

void do_thing(string x) {
    counter += 1;

    cout<<"I just did a thing called "<< x << ", " << counter << " times!\n";
}

int main()
{
    do_thing("THING");
    do_thing("THING");

    return 0;
}
       -}

-- which produces

   {-
I just did a thing called THING, 1 times!
I just did a thing called THING, 2 times!
   -}

-- Modifying some external variable 'in-place' is a side-effect and is not
-- possible in Haskell without explicitly specifying a special structure to
-- allow it.  In fact, even writing output to a terminal is a side-effect, and
-- must use a special data structure to allow it, the IO Monad. Ignore this
-- temporarily.


   {- FUNCTIONS IN HASKELL
       LAMBDA CALCULUS, CURRYING

       A very short overview. See Haskell from First Principles for a longer
       intro.
       -}

-- Every function in haskell can be thought of a composition of smaller
-- functions, each taking one input and giving one output. This is the 'lambda
-- calculus'. https://en.wikipedia.org/wiki/Lambda_calculus

-- You don't need to know much about it, but this structure is what lets us do
-- partial application.

-- Recall this function from the TypeExamples module
checkPower :: Double -> Int -> Double
checkPower x y = x ^ y

-- We can fix the first argument to be 3 and get back another function that
-- returns 3 ^ y. We have 'reduced' the first layer of the function above so
-- that x = 3, and we are left with a function of y.
baseThree :: Int -> Double
baseThree = checkPower 3

-- Another example
squareIt :: Double -> Double
squareIt x = x ^ 2

-- can instead be written more concisely as
squareIt' :: Double -> Double
squareIt' = (^ 2)


-- Currying
-- This method of defining functions with multiple arguments is called Currying
-- https://wiki.haskell.org/Currying

-- It is in contrast to arguments being passed as some collection, such as a
-- tuple or list (as in R), and it allows for exactly the kind of smooth
-- partial application shown above.

-- It also provides the reasoning behind Haskell's type signature syntax
-- Compare the type signature for checkPower (Double -> Int -> Double) with
-- that of baseThree (Int -> Double). The latter is formed by eliminating the
-- first 'Double ->', which corresponds to fixing the x argument to the value
-- 3. You have eliminated the outer layer of the function definition by fixing
-- a value for the first argument.


   {- ANONYMOUS FUNCTIONS -}

-- Confusingly, these are often referred to as 'lambdas', which in the context
-- of 'lambda calculus' might cause you to think they cannot have more than one
-- argument.

-- You'll see genuine reasons to use these when we get to map, filter and local
-- variables, below.

-- These are defined with the following syntax
-- NOTE: there is no type annotation within the lambda syntax itself, so it
-- will be inferred from the context.
-- \input1 input2 ... -> operation

-- This is a silly way to write a function
checkPower' :: Double -> Int -> Double
checkPower' = \x y -> x ^ y

-- a conceptual demonstration of how the lambda calculus works
-- parentheses not needed and are for emphasis
checkPower'' :: Double -> Int -> Double
checkPower'' = \x -> (\y -> x ^ y)



   {- FUNCTION COMPOSITION AND APPLICATION -}

-- (.) is the function composition operator
-- ($) is the function application operator, previewed in the TypeExamples
-- module

-- Conceptually they are similar, as this example shows
-- Here we build a function oneP2 which adds one to a Num-class type, then
-- multiplies it by two. We do so by composing two smaller operations, plusOne
-- and timesTwo.

-- NOTE: there is a funny type-related technicality going on here. We put a literal 1
-- in the definition, but allow a generic Num instance of type a. What happens
-- if a is not Integer, or doesn't have the number 1 as a valid value? I
-- believe what happens is the compiler knows to apply the Num method
-- fromInteger to the literal 1, to get a value of type a, before applying + 1.
-- Perhaps the reader can check on that.

-- I got two type signatures for the price of one
plusOne, timesTwo :: Num a => a -> a
plusOne = (+ 1)
timesTwo = (* 2)

-- With function composition
oneP2 :: Num a => a -> a
oneP2 = timesTwo . plusOne

-- With function application
oneP2' :: Num a => a -> a
-- same as
-- timesTwo (plusOne x)
oneP2' x = timesTwo $ plusOne x


-- Conceptually these are the same, but technically they are not.
-- (.) is an operation on functions, $ is an operation that controls order of
-- function execution.
-- see the type signature for (.), and read ahead to FUNCTIONS AS TYPES
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:.

-- $ is useful in particular for changing the default way in which haskell
-- expressions associate

-- By default, they are left-associative, meaning f x y evaluates as (f x) y
-- This makes a lot of sense in the context of currying.

-- Look back at the baseThree example.
-- It's the exact same as defining it as follows.
-- Left associativity means (checkPower 3) is evaluated first, returning a
-- function Int -> Double.
-- Left associativity works well with currying.
baseThree' :: Int -> Double
baseThree' x = checkPower 3 x

-- What if we wanted to do some transfomation on x, before passing it
-- to checkPower 3?
-- $ allows us to say: evaluate x * 2 first, then pass the result to checkPower 3
doubleThenBaseThree :: Int -> Double
doubleThenBaseThree x = checkPower 3 $ x * 2

-- Without it, we would get this function. It still compiles, but it does the
-- multiplication *after* raising 3 to the power x. That is because of
-- left-associativity.
baseThreeThenDouble :: Int -> Double
baseThreeThenDouble x = checkPower 3 x * 2


   {- FUNCTIONS AS TYPES -}

-- Function signature syntax does more than specify input and output types of a function.
-- They define the **type** of a function itself.
--
-- By that I mean, a function with signature Int -> Int *has type* Int -> Int itself.
-- That type is not the same as the type of a function with signature Double -> Int.

-- The compiler will check these definitions just as it does other types.

-- This function takes as input some function converting Int -> Int, and
-- returns another function with signature Int -> Int by doing it twice.

doItTwice :: (Int -> Int) -> (Int -> Int)
doItTwice f = f . f

-- This won't compile, since baseThree has type Int -> Double
-- bad = doItTwice baseThree

-- You can use this to define type aliases and new types as well
type ItoI = Int -> Int
newtype ItoI2
  = ItoI2 (Int -> Int)

-- NOTE: freaky example showing the clear difference between application and
-- composition this says "do the composition operation on f first, then feed
-- the resulting function to the ItoI2 constructor
doItTwice' :: ItoI2 -> ItoI2
doItTwice' (ItoI2 f) = ItoI2 $ f . f

-- You would then need to use a pattern match to 'unwrap' the function from
-- ItoI2. That's a pain to type, so in a situation as simple as this there is
-- not a very good reason to wrap such a function in a newtype like we did.
unItoI2 :: ItoI2 -> (Int -> Int)
unItoI2 (ItoI2 f) = f

-- POP QUIZ: compare the signature of plusOne to that of ItoI2. What's
-- happening here?
whyOhWhy :: ItoI2
whyOhWhy = ItoI2 plusOne

-- NOTE: recall functions are by default left-associative
-- 3
whatAPain :: Int
whatAPain = unItoI2 whyOhWhy 2


   {- LOCAL VARIABLES with WHERE and LET -}

-- Like many languages, Haskell has a notion of lexical scoping.
princeConstant :: Int
princeConstant = 10

-- I can capture princeConstant within myLair.
myLair :: Int -> Int
myLair x = princeConstant - x

-- Watch out for masking variable names, conflicts etc. Since these are common
-- to all languages with lexical scoping I won't go over it.

-- Local variables: where and let statements

-- define local variables, those living only within the function scope, with
-- where and let keywords. Makes code more readable.

complicatedOp :: (Num a, Show a) => a -> a -> String
complicatedOp x y = show out
   where out = y' + x
         y' = y + 2 * y

-- same as
complicatedOp' :: (Num a, Show a) => a -> a -> String
complicatedOp' x y =
   let out = y' + x
       y' = y + 2 * y
   in show out

-- you can define local *functions* as well
complicatedOp'' :: (Num a, Show a) => a -> a -> String
complicatedOp'' x = show . op2 . op
   -- x in op2 is captured from the complicatedOp'' scope
   where op2 y = y + x
         op y = y + 2 * y


   {- IF-THEN-ELSE and GUARDS -}

-- TODO


   {- MAPS, FOLDS, LIST COMPREHENSIONS -}

-- TODO write explanatory text

-- MAP
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:map
-- map :: (a -> b) -> [a] -> [b]

mapPlusOne :: Num a => [a] -> [a]
mapPlusOne = map plusOne

-- anonymous functions are very useful in the context
squareThem :: Num a => [a] -> [a]
squareThem = map (\x -> x * x)

-- FILTER

-- filter the list for elements bigger than input x
filterBiggerThan :: (Ord a) => a -> [a] -> [a]
filterBiggerThan x = filter (> x)

-- a condensed version
-- (>) x y == x > y
-- flip (>) x y = y > x
filterBiggerThan' :: (Ord a) => a -> [a] -> [a]
filterBiggerThan' = filter . flip (>)

-- LIST COMPREHENSIONS
-- intuitive syntax familiar to python users

mapPlusOne' :: Num a => [a] -> [a]
mapPlusOne' xs = [x + 1 | x <- xs]

filterBiggerThan'' :: (Ord a) => a -> [a] -> [a]
filterBiggerThan'' x xs = [y | y <- xs, y > x]

-- great for combining filters and maps
comboThing :: (Ord a, Num a) => a -> [a] -> [a]
comboThing x xs = [y + 1 | y <- xs, y > x]

-- FOLDS
-- TODO Like Reduce in R. requires some explanation

-- filter, map can be defined in terms of foldl
map' :: (a -> b) -> [a] -> [b]
-- op first argument is the accumulator, whose initial value is []
map' f = foldl op [] where op xs x = f x : xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldl op []
   where op xs x = if p x then x : xs else xs


   {- ADDITIONAL EXAMPLES -}

complete :: [a] -> [(a, a)]
complete xs = map (,) xs <*> xs

complete' :: [a] -> [(a, a)]
complete' xs = [(x, y) | x <- xs, y <- xs]

completeIf :: ((a, a) -> Bool) -> [a] -> [(a, a)]
completeIf p xs = [(x, y) | x <- xs, y <- xs, p (x, y)]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ start []     = start
foldr' f start (x:xs) = f x (foldr' f start xs)

-- EXAMPLE: making a path

-- GOAL: given a list of vertices (represented by any type a) in which each
-- element represents a vertex, return a list of 'edges' given by tuples
-- representing a path from the first element in the list to the last.  e.g.
-- path [1, 2, 3] gives [(1, 2), (2, 3)] and path "word" gives [('w', 'o'),
-- ('o', 'r'), ('r', 'd')]. Convention is path [1] = [(1, 1)]

-- NOTE these differ in how they handle singletons, in their constraints and in
-- how they handle uniqueness in the argument list
path, path', path'' :: Eq a => [a] -> [(a, a)]
-- with foldr
path = foldr join []
  where
    join x []          = [(x, x)]
    join x ((y, z):ys) = if y == z then (x, y) : ys else (x, y) : (y, z) : ys

-- with recursion
path' [] = []
path' [x] = []
path' (x:xs) = join x xs : path' xs
  -- NOTE this first case is unreachable
  where join y []     = (y, y)
        join y (z:zs) = (y, z)

-- first argument is kept in case of duplicates
path'' = path' . nub


-- path permutations
-- permutations is from Data.List
-- Q: why do we need the typeclass constraint Eq?

pathPermutations :: Eq a => [a] -> [[(a, a)]]
pathPermutations = map path' . permutations . nub

-- this one converts to a path first, then 'permutes' it
-- e.g.
-- rot (1, 2) [(2, 3)] = [(1, 2), (2, 3)] : map (attach 2) (rot (1, 3) [])
--                     = [(1, 2), (2, 3)] : map (attach 2) [[(1, 3)], [(3, 1)]]
--                     = [(1, 2), (2, 3)] : [[(2, 1), (1, 3)], [(2, 3), (3, 1)]]
pathPermutations' :: Eq a => [a] -> [[(a, a)]]
pathPermutations' = foldr (concatMap . rot) [[]] . path''
  where
    rot (x, y) [] = [[(x, y)], [(y, x)]]
    -- NOTE: you presume _ and y are the same value
    rot (x, _) ((y, z) : zs) = ((x, y) : (y, z) : zs) : map (attach y) (rot (x, z) zs)
    attach y []            = [(y, y)]
    attach y ((z, u) : us) = (y, z) : (z, u) : us

adHocCheck :: Bool
thing = "kjhasdaaa"
adHocCheck = setEquals (pathPermutations thing) (pathPermutations' thing)

-- pathPermutations == pathPermutations' . path''
-- or rather (because of different orderings) they are equal as sets
-- note this is slow for lists of length n! for any reasonably sized n
setEquals :: Eq b => [b] -> [b] -> Bool
setEquals xts yts = xts `intersect` yts == yts
