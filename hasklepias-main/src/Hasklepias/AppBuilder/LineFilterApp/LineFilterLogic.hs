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
a few common abbreviations are:

- `put`: function that outputs a result in IO ()
- `pid`: function that parses a line into a group identifier
- `psl`: function that parses a line into a `Maybe a`
- `prd`: predicate on type `a`

-}

module Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
  ( filterGroupFold
  , filterAppFold
  , filterAppFoldM
  , filterAppC
  , filterAppFold'
  , filterAppC'
  ) where

import           Conduit
import qualified Control.Foldl                 as L
import qualified Data.ByteString.Char8         as C
import           Data.Conduit.Combinators      as CC
                                                ( filter
                                                , linesUnboundedAscii
                                                , unlinesAscii
                                                )
import           Data.Conduit.List             as CL
                                                ( groupBy )
import           Data.String                    ( IsString )

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
filterGroupStep
  :: (Semigroup t, IsString t, Eq t)
  => (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (GroupState t -> t -> GroupState t)
filterGroupStep prl prd (b, x) y = case (b, x == "") of
  (True , _    ) -> (b, unline x y)
  (False, True ) -> (parseThenPredicate prl prd y, y)
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
filterGroupDoneIO put (b, x) = if b then put x else pure ()

-- Defines the Control.Foldl version of the within group fold process.
filterGroupFold
  :: (Semigroup t, IsString t, Eq t)
  => (t -> Maybe a)
  -> (a -> Bool)
  -> L.Fold t t
filterGroupFold prl prd = L.Fold step begin done
 where
  begin = filterGroupBegin
  step  = filterGroupStep prl prd
  done  = filterGroupDone
{-# INLINE filterGroupFold #-}

{-
  Line processing fold logic (non-monadic version)
  Defines the fold for a stream (across groups) of lines.

  See NOTE:logic-benchmarks 
  for discussion on performance of the implementations 
  of the application logic.
-}

-- (Current Group ID, GroupState, Accumulator)
type LinesAppState a t = (a, (Bool, t), t)

{-
Process a new line.
If the current identifier matches the previous identifer,
continue processing the group.
Otherwise, unload the previous group into the accumulator,
reset the identifier with the current,
and start processing a new group.
-}
filterAppStep
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (LinesAppState i t -> t -> LinesAppState i t)
filterAppStep pid psl prd (prevId, x, acc) y = do
  let currId = pid y
  if prevId == currId
    then (prevId, filterGroupStep psl prd x y, acc)
    else
      let prevGroup = filterGroupDone x
      in  ( currId
          , (parseThenPredicate psl prd y, y)
          , if acc == "" then prevGroup else unline acc prevGroup
          )
{-# INLINE filterAppStep #-}

-- Begin step for line process app
filterAppBegin :: (IsString t) => (t -> i) -> LinesAppState i t
filterAppBegin pid = (pid "", filterGroupBegin, "")

-- Done step for line process app
filterAppDone :: (Semigroup t, IsString t) => LinesAppState i t -> t
filterAppDone (_, lastGroup, x) = unline x (filterGroupDone lastGroup)

-- | `Control.Foldl.Fold` version of the line filter application logic.
filterAppFold
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate 
  -> L.Fold t t
filterAppFold pid psl prd = L.Fold step begin done
 where
  step  = filterAppStep pid psl prd
  begin = filterAppBegin pid
  done  = filterAppDone
{-# INLINE filterAppFold #-}

filterAppFold'
  :: Eq i
  => (C.ByteString -> i)
  -> (C.ByteString -> Maybe a)
  -> (a -> Bool)
  -> C.ByteString
  -> C.ByteString
filterAppFold' pid psl prd x = L.fold (filterAppFold pid psl prd) (C.lines x)
{-# INLINE filterAppFold' #-}

{-
  Line processing fold logic
  Defines the fold for a stream (across groups)
  for use in a context where IO can be done

  See NOTE:logic-benchmarks 
  for discussion on performance of the implementations 
  of the application logic.
-}

-- (Current Group ID, GroupState)
-- This version does not have an accumulator,
-- as data may be output at each step.
type LinesAppStateM a t = (a, (Bool, t))

{-
A version of filterAppStep that outputs the previous group
when the identifier changes.
-}
filterAppStepM
  :: (Monad m, Eq i, Semigroup t, IsString t, Eq t)
  => (t -> m ()) -- ^ output function
  -> (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (LinesAppStateM i t -> t -> m (LinesAppStateM i t))
filterAppStepM put pid psl prd (i, x) y = do
  let newId = pid y
  if i == newId
    then pure (i, filterGroupStep psl prd x y)
    else
      (do
        filterGroupDoneIO put x
        pure (newId, (parseThenPredicate psl prd y, y))
      )
{-# INLINE filterAppStepM #-}

-- Begin step for line process app in monadic context
filterAppBeginM
  :: (Applicative m, IsString t) => (t -> i) -> m (i, GroupState t)
filterAppBeginM pid = pure (pid "", filterGroupBegin)

-- Done step for line process app in monadic context
filterAppDoneM :: Applicative m => (t -> m ()) -> LinesAppStateM i t -> m ()
filterAppDoneM put (i, x) = filterGroupDoneIO put x

-- | `Control.Foldl.FoldM` version of the line processing application's logic.
filterAppFoldM
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> IO ())
  -> (t -> i)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> L.FoldM IO t ()
filterAppFoldM put pid psl prd = L.FoldM step begin done
 where
  step  = filterAppStepM put pid psl prd
  begin = filterAppBeginM pid
  done  = filterAppDoneM put
{-# INLINE filterAppFoldM #-}

{-
  Conduit-based filter application logic

  See NOTE:logic-benchmarks 
  for discussion on performance of the implementations 
  of the application logic.
-}

-- | `Conduit` version of the line processing application's logic.
filterAppC
  :: (Eq i, Monad m)
  => (C.ByteString -> i)  -- ^ identifier parser
  -> (C.ByteString -> Maybe a)  -- ^ parsing function
  -> (a -> Bool)  -- ^ predicate
  -> C.ByteString
  -> ConduitT a2 o m C.ByteString
filterAppC pid psl prd x =
  yield x
    .| CC.linesUnboundedAscii
    .| CL.groupBy (\x y -> pid x == pid y)
    .| mapC (L.fold (filterGroupFold psl prd))
    .| CC.filter (/= "")
    .| CC.unlinesAscii
    .| foldC

filterAppC' f g h x = runConduitPure $ filterAppC f g h x



{-
NOTE:logic-benchmarks

The following are benchmarking results using
[hyperfine](https://github.com/sharkdp/hyperfine)
on command line applications based on conduit (via `filterAppC`)
and on `Control.FoldL` (via `filterAppFold`).
Note that `filterAppC` does use `FoldL` for the group-level fold.
Input files were generated in `Test.Hasklepias.LineFilterApp`.
Inputs files not ending in "allfail" mean that all the groups have 
one line that passed the predicate.

Results for 10/100000 groups are based on 3 runs;
all others are based on 10 runs.

| Input                              | conduit             | foldl                |
|------------------------------------| ------------------- | -------------------- |
| 10groups-100000lines.jsonl         | 17.758 s ±  1.948 s | 15.448 s ±  0.064 s  |
| 100groups-10000lines.jsonl         | 2.368 s ±  0.068 s  | 1.915 s ±  0.063 s   |
| 1000groups-1000lines.jsonl         | 1.956 s ±  0.045 s  | 1.244 s ±  0.006 s   |
| 10000groups-100lines.jsonl         | 5.984 s ±  0.106 s  | 3.100 s ±  0.008 s   |
| 100000groups-10lines.jsonl         | 63.932 s ±  7.077 s | 27.905 s ±  0.166 s  |
| 10groups-100000lines-allfail.jsonl | 15.511 s ±  0.023 s | 15.365 s ±  0.203    |
| 100groups-10000lines-allfail.jsonl | 2.298 s ±  0.007 s  | 1.844 s ±  0.017 s   |
| 1000groups-1000lines-allfail.jsonl | 1.296 s ±  0.006 s  | 788.0 ms ±   4.4 ms  |
| 10000groups-100lines-allfail.jsonl | 1.141 s ±  0.004 s  | 674.9 ms ±   4.2 ms  |
| 100000groups-10lines-allfail.jsonl | 1.164 s ±  0.003 s  | 685.5 ms ±   8.3 ms  | 



-}
