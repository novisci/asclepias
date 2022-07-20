{-|
Core functionality for processing new line delimited data

In the context of creating cohorts from event lines,
the functionality herein may be useful when prefiltering events
in order to reduce the size of the input data.
For example, if the cohort is consists only of females,
one could run a prefilter to remove any groups (subjects in this case)
of event lines for males.

-}
module Hasklepias.AppBuilder.ProcessLines.Logic
  ( -- * Processing a single group's lines
    -- 
    -- $processGroupLines
    processGroupLinesStrict
  , processGroupLinesLazy

    -- * Processing multiple groups
    --
    -- $processAppLines
  , processAppLinesStrict
  , processAppLinesLazy
  ) where

import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Int
import           Data.Maybe
import qualified Data.Text                     as T
import           Debug.Trace
{-
INTERNAL
Run a parser then a predicate, 
returning `False` if the parsing failed.
-}
parseThenPredicate :: (t -> Maybe a) -> (a -> Bool) -> t -> Bool
parseThenPredicate psl prd = maybe False prd . psl

{-
INTERNAL
A data type containing functions used in the line processing fucnctions.
The purpose of this type is be able to create versions
of the process* functions for both (e.g.)
strict and lazy bytestrings
without creating multiple versions of the same logic.
The index type for strict ByteString is Int,
but for lazy ByteString is Int64.
Hence we run into the issue of needing
functions specific to each string type.
I (BS) could not find a typeclass
that provides the functionality needed.
-}
data LineFunctions t i = MkLineFunctions
  { isEmpty     :: t -> Bool
  , findNewLine :: t -> Maybe i
  , takeSubset  :: t -> i -> Maybe i -> t
  , build       :: t -> Builder
  , runBuilder  :: Builder -> t
  }

lineFunctionsStrict :: LineFunctions BS.ByteString Int
lineFunctionsStrict = MkLineFunctions
  { isEmpty     = BS.null
  , takeSubset  = \x i n -> case n of
                    Nothing -> BS.drop i x
                    Just j  -> BS.take j (BS.drop i x)
  , findNewLine = BSC.elemIndex '\n'
  , build       = byteString
  , runBuilder  = BL.toStrict . toLazyByteString
  }

lineFunctionsLazy :: LineFunctions BL.ByteString Int64
lineFunctionsLazy = MkLineFunctions
  { isEmpty     = BL.null
  , takeSubset  = \x i n -> case n of
                    Nothing -> BL.drop i x
                    Just j  -> BL.take j (BL.drop i x)
  , findNewLine = BLC.elemIndex '\n'
  , build       = lazyByteString
  , runBuilder  = toLazyByteString
  }


{-------------------------------------------------------------------------------
   Within-group fold logic

A means of deciding whether include a group in the output
and returning a group's data unchanged if it is to be output.
A group is output if at least one of its elements
both successfully parses (given a parser) and
satisfies a given predicate.
Input elements that fail to parse are included in the output.

See NOTE:combine-group-app
-------------------------------------------------------------------------------}

{- 
INTERNAL
Data carrying information about whether 
the predicate has been satisfied for a group
and 
the index of the last new line character.
-}
data GrpLines i = MkGrpLines
  { predicateSatified :: Bool
  , lastNewLine       :: Maybe i
  }
  deriving (Eq, Show)

{-
INTERNAL
The core recursive logic of processing the lines within a group.
The function "rolls" over the *single string* of a group, 
updating a `GrpLines` values as it encounters new line characters
within that string.
-}
processGroupLinesInternal
  :: (Num i)
  => LineFunctions t i
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> GrpLines i
  -> t
  -> GrpLines i
processGroupLinesInternal fs psl prd status x
  | isEmpty fs x || predicateSatified status = status
  | otherwise = case lastNewLine status of
    Nothing -> status
    -- If there was a new line character at the previous iteration,
    -- see if there is another new line character *after*
    -- the previous new line character, handling cases as follows:
    --  * If there is a previous and current new line character,
    --    then update the `GrpLines` tracker 
    --    with the value of the predicate from the current line
    --    and the index of the current new line character 
    --    (relative to whole input) and continue.
    --   * If there is a previous new line but no current new line,
    --     then update the `GrpLines` tracker and continue.
    Just i ->
      let getTailTo = takeSubset fs x (i + 1)
      in  let nl = findNewLine fs (getTailTo Nothing)
          in  go (getTailTo nl) (fmap (\n -> n + i + 1) nl)
 where
  go i j = processGroupLinesInternal
    fs
    psl
    prd
    (MkGrpLines (parseThenPredicate psl prd i) j)
    x
{-# INLINE processGroupLinesInternal #-}

{-
INTERNAL
The function used to create a processGroupLines* function
targeted for a specific type. 
-}
processGroupLines
  :: (Num i, Monoid t)
  => LineFunctions t i
  -> (t -> Maybe a) -- ^ parser for type `a` to extract from a line 
  -> (a -> Bool) -- ^ predicate on type `a`
  -> t -- ^ single string for a group's lines
  -> t
processGroupLines fs psl prd x =
  let status = predicateSatified $ processGroupLinesInternal
        fs
        psl
        prd
        (MkGrpLines False (Just (-1)))
        x
  in  if status then x else mempty

{- $processGroupLines

The @processGroupLines*@ functions take
all of a group's lines as single input.
The function identifies whether *any* line within the group
satisfy a predicate.
If so, then the group's lines are returned unchanged;
otherwise, an empty string is returned.

For each line in the input,
defined as the substring between @'\n'@ characters,
the provided parser and predicate are applied.
Given the parser of type @t -> Maybe a@,
a line may be parsed in a type @a@. 
If the parse is successful, 
then predicate (@a -> Bool@) is applied;
otherwise, a line results in @False@ by default.

Three variants of @processGroupLines*@ are available:

* 'processGroupLinesStrict' works on strict 'BS.ByteString'. 
This version is generally faster, 
but requires that the entire input be loaded into memory.
* 'processGroupLinesLazy' works on lazy 'BL.ByteString'.
* 'processGroupLinesText' works on 'Data.Text.Text'
and mostly useful demostrating the functionality (as below).

The following is a trivial example of the logic at work.
The data are newline delimited integers.
The parser simply parses the strings @"1"@ and @"2"@,
and the predicate checks whether the value is @1@.

>>> let allOnes = "1\n1\n1"
>>> let allTwos = "2\n2\n2"
>>> let i321 = "3\n2\n1"
>>> let i123 = "1\n2\n3" 
>>> readOne x = if x == "1" then Just 1 else (if x == "2" then Just 2 else Nothing)
>>> processGroupLinesStrict readOne (== 1) allOnes 
"1\n1\n1"
>>> processGroupLinesStrict readOne (== 1) allTwos
""
>>> processGroupLinesStrict readOne (== 1) i321
"3\n2\n1"
>>> processGroupLinesStrict readOne (== 1) i123
"1\n2\n3"
-}

-- | Process a group of strict 'BS.ByteString'.
processGroupLinesStrict
  :: (BS.ByteString -> Maybe a) -> (a -> Bool) -> BS.ByteString -> BS.ByteString
processGroupLinesStrict = processGroupLines lineFunctionsStrict

-- | Process a group of lazy 'BL.ByteString'.
processGroupLinesLazy
  :: (BL.ByteString -> Maybe a) -> (a -> Bool) -> BL.ByteString -> BL.ByteString
processGroupLinesLazy = processGroupLines lineFunctionsLazy

{-------------------------------------------------------------------------------
   Across-group fold ("application") logic

This application processes streams of newline delimited text at two levels:

(1) Groups of lines defined by a parser. 
For example, if the input is event lines data,
the application groups the input events by subject ID. 

(2) Processing each group

See NOTE:combine-group-app
-------------------------------------------------------------------------------}

{- 
INTERNAL
Data carrying information about  
* the group ID at the previous line
* the index at which the current group started
* the index of the last new line
* the accumulated results as a ByteString Builder
-}

data AppLines id i = MkAppLines
  { lastID       :: Maybe id
  , groupStart   :: i
  , lastNewLine' :: Maybe i
  , builder      :: Builder
  }

{-
INTERNAL
The core recursive logic of processing the lines across groups.
The function "rolls" over the *single string* of new line delimited data 
updating an `AppLines` value as it encounters new line characters
within that string.

IMPORTANT
All the lines for each group are assumed to be contiguous.
-}
processAppLinesInternal
  :: (Eq id, Show id, Num i, Monoid t)
  => LineFunctions t i
  -> (t -> Maybe id)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> AppLines id i
  -> t
  -> AppLines id i
processAppLinesInternal fs pri psl prd status x
  | isEmpty fs x = status
  | otherwise = case fmap (+ 1) (lastNewLine' status) of
    -- If no new line then we're done!
    Nothing -> status
    -- Otherwise take the index immediately after the newline character as `i`
    Just i  -> do

      let getTail   = takeSubset fs x i
      -- Is there another newline after `i`?
      let nl        = findNewLine fs (getTail Nothing)

      -- Identify the groupID in the subset of x
      -- in the interval [i, i + n].
      let currentID = pri (getTail nl)

      -- When the ID has not changed, 
      -- just update the index of the last newline.
      -- When the ID does change,
      -- then process the group for the last ID
      -- and update the ID in the accumulator

      let newPos    = fmap (i +) nl
      if currentID == lastID status
        then go $ status { lastNewLine' = newPos }
        else go $ MkAppLines
          currentID
          i
          newPos
          (builder status <> build
            fs
            (processGroup
              (takeSubset fs
                          x
                          (groupStart status)
                          (Just (i - groupStart status))
              )
            )
          )
 where -- the recursion 
  go           = flip (processAppLinesInternal fs pri psl prd) x
  processGroup = processGroupLines fs psl prd
{-# INLINE processAppLinesInternal #-}


{-
INTERNAL
The function used to create a processAppLines* function
targeted for a specific type. 
-}
processAppLines
  :: (Eq id, Show id, Num i, Monoid t, Show i, Show t)
  => LineFunctions t i
  -> (t -> Maybe id)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> t
  -> t
processAppLines fs pri psl prd x = runBuilder fs $ builder2
  (processAppLinesInternal2 fs
                            pri
                            psl
                            prd
                            (MkAppLines2 Nothing (0) False (Just (-1)) mempty)
                            x
  )

{- $processAppLines

The @processAppLines*@ functions effectively split a string 
into groups identified by a value parsed from each line, 
then apply @'processGroupLines'@ to each group.

IMPORTANT: 
All the lines for each group should be contiguous in the input.

Two variants of @processGroupLines*@ are available:

* 'processAppLinesStrict' works on strict 'BS.ByteString'. 
This version is generally faster, 
but requires that the entire input be loaded into memory.
* 'processAppLinesLazy' works on lazy 'BL.ByteString'.

These functions are difficult to demostrate succinctly.
For a complete example, 
see the source code in @Hasklepias.AppBuilder.ProcessLines.Tests@.

-}

-- | Process a strict 'BS.ByteString'.
processAppLinesStrict
  :: (Eq id, Show id)
  => (BS.ByteString -> Maybe id)
  -> (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> BS.ByteString
  -> BS.ByteString
processAppLinesStrict = processAppLines lineFunctionsStrict

-- | Process a lazy 'BL.ByteString'.
processAppLinesLazy
  :: (Eq id, Show id)
  => (BL.ByteString -> Maybe id)
  -> (BL.ByteString -> Maybe a)
  -> (a -> Bool)
  -> BL.ByteString
  -> BL.ByteString
processAppLinesLazy = processAppLines lineFunctionsLazy



{-
INTERNAL
The core recursive logic of processing the lines across groups.
The function "rolls" over the *single string* of new line delimited data 
updating an `AppLines` value as it encounters new line characters
within that string.

IMPORTANT
All the lines for each group are assumed to be contiguous.
-}

data AppLines2 id i = MkAppLines2
  { lastID2        :: Maybe id
  , groupStart2    :: i
  , predSatisfied2 :: Bool
  , lastNewLine2   :: Maybe i
  , builder2       :: Builder
  }


processAppLinesInternal2
  :: (Eq id, Show id, Num i, Monoid t, Show i, Show t)
  => LineFunctions t i
  -> (t -> Maybe id)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> AppLines2 id i
  -> t
  -> AppLines2 id i
processAppLinesInternal2 fs pri psl prd status x
  | isEmpty fs x = status
  | otherwise = case fmap (+ 1) (lastNewLine2 status) of
    -- If no new line then we're done!
    Nothing ->
      -- trace (show ("e", predSatisfied2 status, groupStart2 status))
               status
      { builder2 = if predSatisfied2 status
                     then builder2 status <> build
                       fs
                       (takeSubset fs x (groupStart2 status) Nothing)
                     else builder2 status
      }
    -- Otherwise take the index immediately after the newline character as `i`
    Just i -> do

      let getTail   = takeSubset fs x i
      -- Is there another newline after `i`?
      let nl        = findNewLine fs (getTail Nothing)

      -- Identify the groupID in the subset of x
      -- in the interval [i, i + n].
      let currentID = pri (getTail nl)

      -- When the ID has not changed, 
      -- just update the index of the last newline.
      -- When the ID does change,
      -- then process the group for the last ID
      -- and update the ID in the accumulator

      let newPos    = fmap (i +) nl

      let newStatus = status { lastNewLine2 = newPos }

      case (currentID == lastID2 status, predSatisfied2 status) of
        (True, True) -> go newStatus
        (True, False) ->
          go newStatus { predSatisfied2 = processLine (getTail nl) }
        (False, True) -> go newStatus
          { lastID2        = currentID
          , predSatisfied2 = processLine (getTail nl)
          , groupStart2    = i
          , builder2       = updateBuilder  status (Just i)
            
            -- builder2 status <> build
            --                    fs
            --                    (takeSubset fs
            --                                x
            --                                (groupStart2 status)
            --                                (Just (i - groupStart2 status))
            --                    )
          }
        (False, False) -> trace
          (show ("d", predSatisfied2 status, groupStart2 status, lastID2 status)
          )
          go
          newStatus { lastID2        = currentID
                    , groupStart2    = i
                    , predSatisfied2 = processLine (getTail nl)
                    }
 where -- the recursion 
  go          = flip (processAppLinesInternal2 fs pri psl prd) x
  processLine = parseThenPredicate psl prd
  updateBuilder status i = builder2 status <> build fs (takeSubset fs x (groupStart2 status) (fmap (\z -> z - groupStart2 status) i))
{-# INLINE processAppLinesInternal2 #-}


{-
NOTE:combine-group-app

The current implementation of processAppLinesInternal 
essentially does two passes over a group's lines:
one to identify a group then a second pass to apply processGroupLines.
As an optimization, these two operations could (should?) be fused into one pass.
This optimization was not implemented in
https://gitlab.novisci.com/nsStat/asclepias/-/merge_requests/245
because the processAppLines function is many times faster and more efficiently
than the previous version, 
and I (B. Saul) decided effort was better spent elsewhere at the time.
-}
