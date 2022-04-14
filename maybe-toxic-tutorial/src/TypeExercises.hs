-- IGNORE: i have explicitly turned off certain compiler warnings.
{-# HLINT ignore #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE run ghci with src as current working directory, else you will get a
-- failure to find TypeTribExamples module
module TypeExercises
  () where

import           TypeExamples                   ( Hour(..)
                                                , Tree
                                                )

-- EX 1
-- write a function that adds 3 to any instance of the typeclass Num
-- add the appropriate type signature to reflect this
-- addThree :: ...
addThree = undefined

-- It might be unclear from docs that ^ requires x and output x ^ k to be same
-- type it's basic math though that some power of a non-integer number might
-- not give back an integer. Typer-checker encodes that for us.

-- EX 2
-- This type signature has two problems.
-- Fix it for the *particular* case where the input is Double
-- by looking at the type signature for (^)
-- uncomment and try to load this file into ghci (or cabal build) to see if it
-- compiles
  {-
doublePower :: Double -> Double
doublePower = (^)
-}

-- EX 3
-- Write a version of toNatlTrunc that is generic over all
-- types that be converted to Integer.
-- Hint: there is a typeclass for exactly this.
-- toNatlTrunc' :: ???
-- toNatlTrunc' x = ???


-- EX 4
-- This function will compile but will *always return a run-time error*
-- Look at the docs to see why (possibly following the Source link for clarification) then fix the function by making a minor change.
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:-94-
negativePower :: Num a => a -> a
negativePower x = x ^ (-1)


-- EX 5
-- the Prelude module is implicitly loaded by default
-- Use the Prelude documatation for List to write a function that returns the
-- third element of any list, with a run-time error if the list does not have
-- three or more elements
-- indexing in haskell starts at 0
third :: [a] -> a
third = undefined


-- EX 5
-- use the Maybe type to make a 'safe' version of your function third, which returns nothing instead of producing an error when the list has fewer than three elements
safeThird :: [a] -> Maybe a
safeThird = undefined

-- use Maybe type again to make a 'safe' version of
-- listToTree, which returns Just (Tree a) if called on a
-- non-empty list and Nothing otherwise.
listToTree' :: [a] -> Maybe (Tree a)
listToTree' = undefined


-- More type classes

-- EX 6
-- implement Hour as an instance of Eq, meaning types for which you can define ==
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Eq
-- Hour a == Hour b if a is equal to b modulo 12
-- use the docs to see what functions you need to define to declare Hour as an instance of typeclass Eq

-- Q: How is this different from what we would get it we used deriving Eq in the type declaration, as we did with Show?
instance Eq Hour where
  (==) = undefined

-- EX 7
-- implement Hour as an instance of Ord, meaning types that can be sequentially ordered
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#t:Ord
-- gives the minimal complete definition as compare or <=. It's probably easiest to define the operation <= in a way similar to how (<>) was defined: by appropriately comparing the values x, y that are backing up the Hour type
instance Ord Hour where
  (<=) = undefined
