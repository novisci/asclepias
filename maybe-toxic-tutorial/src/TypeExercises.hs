-- NOTE run ghci with src as current working directory, else you will get a
-- failure to find TypeTribExamples module
module TypeExercises ( ) where

import           TypeExamples (Hour)

-- write a function that adds 3 to any instance of the typeclass Num
-- add the appropriate type signature to reflect this
-- addThree :: ...
addThree = undefined

-- It might be unclear from docs that ^ requires x and output x ^ k to be same
-- type it's basic math though that some power of a non-integer number might
-- not give back an integer. Typer-checker encodes that for us.

-- This type signature has two problems.
-- Fix it for the *particular* case where the input is Double
-- by looking at the type signature for (^)
-- uncomment and try to load this file into ghci (or cabal build) to see if it
-- compiles
  {-
doublePower :: Double -> Double
doublePower = (^)
-}


-- This function will compile but will *always return a run-time error*
-- Look at the docs to see why (possibly following the Source link for clarification) then fix the function by making a minor change.
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:-94-
negativePower :: Num a => a -> a
negativePower = (^ (-1))


-- the Prelude module is implicitly loaded by default
-- Use the Prelude documatation for List to write a function that returns the
-- third element of any list, with a run-time error if the list does not have
-- three or more elements
-- indexing in haskell starts at 0
third :: [a] -> a
third = undefined


-- use the Maybe type to make a 'safe' version of your function third, which returns nothing instead of producing an error when the list has fewer than three elements
safeThird :: [a] -> Maybe a
safeThird = undefined


-- Type classes
-- implement Hour as an instance of Eq, meaning types for which you can define ==
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Eq
-- Hour a == Hour b if a is equal to b modulo 12
-- use the docs to see what functions you need to define to declare Hour as an instance of typeclass Eq

-- Q: How is this different from what we would get it we used deriving Eq in the type declaration, as we did with Show?
instance Eq Hour where
  (==) = undefined

-- implement Hour as an instance of Ord, meaning types that can be sequentially ordered
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Ord
-- gives the minimal complete definition as compare or <=. It's probably easiest to define the operation <= in a way similar to how (<>) was defined: by appropriately comparing the values x, y that are backing up the Hour type
instance Ord Hour where
  (<=) = undefined
