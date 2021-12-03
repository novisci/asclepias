module FunctionalExamples ( ) where

import           Data.List (nub, permutations)
  {--
     WHAT YOU WILL LEARN

     PRE-REQUISITES

     --}

{-
  TODO: intro something with resources
  -}


-- TODO function call basics

-- TODO currying, lambda functions, composition and application

-- TODO pattern matching, case matching syntax variants, where, let, guards,
-- esp. list pattern matching

-- TODO recursions

-- TODO maps, folds, list comprehensions, and related typeclasses

complete :: [a] -> [(a, a)]
complete xs = map (,) xs <*> xs

complete' :: [a] -> [(a, a)]
complete' xs = [(x, y) | x <- xs, y <- xs]

completeIf :: ((a, a) -> Bool) -> [a] -> [(a, a)]
completeIf p xs = [(x, y) | x <- xs, y <- xs, p (x, y)]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ start []     = start
foldr' f start (x:xs) = f x (foldr' f start xs)

-- making a path

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
    attach y [] = [(y, y)]
    attach y ((z, u) : us) = (y, z) : (z, u) : us

adHocCheck :: Bool
x = "kjhasdaaa"
adHocCheck = setEquals (pathPermutations x) (pathPermutations' x)

-- pathPermutations == pathPermutations' . path''
-- or rather (because of different orderings) they are equal as sets
-- note this is slow for lists of length n! for any reasonably sized n
setEquals :: Eq b => [b] -> [b] -> Bool
setEquals xts yts = all (`elem` xts) yts && all (`elem` yts) xts
