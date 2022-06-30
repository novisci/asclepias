{-|
Core logic of Filter Application

This application processes streams of text at two levels:

(1) A means of grouping the stream. 
For example, if the input is event lines data,
the application groups the input events by subject ID. 

(2) A means of deciding whether include a group in the output
and returning a group's data unchanged if it is to be output.
A group is output if at least one of its elements
both successfully parses (given a parser) and
satisfies a given predicate.
Input elements that fail to parse are included in the output.

In the functions below,
a few common shorthands are:

- `put`: function that outputs a result in IO ()
- `pid`: function that parses a line into a group identifier
- `psl`: function that parses a line into a `Maybe a`
- `prd`: predicate

-}

module Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic where

import Data.String ( IsString )
import qualified Control.Foldl as L

-- INTERNAL
-- Run a parser then a predicate, 
-- returning `False` if the parsing failed.
parseThenPredicate :: (t -> Maybe a) -> (a -> Bool) -> t -> Bool
parseThenPredicate psl prd = maybe False prd . psl

-- INTERNAL
-- Combine two strings, delimited by newline character.
unline :: (Semigroup t, IsString t) => t -> t -> t
unline x y = x <> "\n" <> y

{-
   Within-group fold logic
-}
type GroupState t = (Bool, t)

-- Fold step for within a group.
filterGroupStep :: (Semigroup t, IsString t, Eq t) =>
     (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (GroupState t -> t -> GroupState t)
filterGroupStep prl prd (b, x) y =
    case (b, x == "") of
      (True, _) -> (b, unline x y)
      (False, True)  -> (parseThenPredicate prl prd y, y)
      (False, False) -> (parseThenPredicate prl prd y, unline x y)
{-# INLINE filterGroupStep #-}

-- Initial step of a group-level fold
filterGroupBegin :: IsString t => GroupState t
filterGroupBegin = (False, "")

-- Extraction step of a group-level fold
filterGroupDone :: IsString t => GroupState t -> t
filterGroupDone (b, x) = if b then x else ""

-- Extraction step of group-level fold within a monad with IO capabailities
-- Allows for result to be output in IO at end of group's fold.
filterGroupDoneIO :: Applicative m => (t -> m ()) -> GroupState t -> m ()
filterGroupDoneIO put (b, x) = if b then  put x else pure ()

-- Defines the Control.Foldl version of the within group fold process.
filterGroupFold :: (Semigroup t, IsString t, Eq t) => 
     (t -> Maybe a) 
  -> (a -> Bool) 
  -> L.Fold t t
filterGroupFold prl prd = L.Fold step begin done 
  where begin = filterGroupBegin
        step  = filterGroupStep prl prd
        done  = filterGroupDone 
{-# INLINE filterGroupFold #-}

{-
   Line processing fold logic
   Defines the fold for a stream (across groups)
-}
type LinesAppState a t = (a, (Bool, t))

{-
The logic of the application's fold step,
Given a way to 
(1) output results
(2) parse an identifier from a line
(3) parse an `a` from a line
(4) apply a predicate to an `a`
this function returns a function that
takes the current state and new line
to return a new state within a monadic context.
If the new line has a diffenent identifier, 
then the current state is output and 
-}
filterAppStepM :: (Monad m, Eq i, Semigroup t, IsString t, Eq t) =>
     (t -> m ()) -- ^ output function
  -> (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (LinesAppState i t -> t -> m (LinesAppState i t))
filterAppStepM put pid psl prd (i, x) y =
  do
    let newId = pid y
    if i == newId then
      pure (i, filterGroupStep psl prd x y) 
    else (do
      filterGroupDoneIO put x
      pure (newId, (parseThenPredicate psl prd y, y)))
{-# INLINE filterAppStepM #-}

-- Begin step for line process app in monadic context
filterAppBeginM :: (Applicative m, IsString t) =>
  (t -> i) -> m (i, GroupState t)
filterAppBeginM pid = pure (pid "", filterGroupBegin) 

-- Done step for line process app in monadic context
filterAppDoneM :: Applicative m => (t -> m ()) -> LinesAppState i t -> m ()
filterAppDoneM put (i, x) = filterGroupDoneIO put x 

-- Control.Foldl version of the line processing application's fold
filterAppFoldM :: (Eq i, Semigroup t, IsString t, Eq t) =>
     (t -> IO ())
  -> (t -> i)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> L.FoldM IO t ()
filterAppFoldM put pid psl prd = L.FoldM step begin done
  where 
    step = filterAppStepM put pid psl prd
    begin = filterAppBeginM pid
    done = filterAppDoneM put
{-# INLINE filterAppFoldM #-}