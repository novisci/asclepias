{-|
Core logic of processing lines
-}

module Hasklepias.AppBuilder.ProcessLines.Logic
  ( -- * Processing a single group's lines
    -- 
    -- $processGroupLines
    processGroupLinesStrict
  , processGroupLinesLazy
  ) where


import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Int
import qualified Data.Text                     as T
import           GHC.Exts                       ( IsList(fromList)
                                                , IsString
                                                )
-- import Debug.Trace

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
  { drop'       :: i -> t -> t
  , isEmpty     :: t -> Bool
  , findNewLine :: t -> Maybe i
  , takeEnd     :: t -> i -> t
  , takeSubset  :: t -> i -> i -> t
  }

lineFunctionsStrict :: LineFunctions B.ByteString Int
lineFunctionsStrict = MkLineFunctions
  { isEmpty     = BS.null
  , drop'       = BS.drop
  , takeEnd     = \x n -> BS.drop (BS.length x - n) x
  , takeSubset  = \x i n -> BS.take n (BS.drop (i + 1) x)
  , findNewLine = BS.elemIndex '\n'
  }

lineFunctionsLazy :: LineFunctions BL.ByteString Int64
lineFunctionsLazy = MkLineFunctions
  { isEmpty     = BL.null
  , drop'       = BL.drop
  , takeEnd     = \x n -> BL.drop (BL.length x - n) x
  , takeSubset  = \x i n -> BL.take n (BL.drop (i + 1) x)
  , findNewLine = BL.elemIndex (B.c2w '\n')
  }

lineFunctionsText :: LineFunctions T.Text Int
lineFunctionsText = MkLineFunctions
  { isEmpty     = T.null
  , drop'       = T.drop
  , takeEnd     = \x n -> T.drop (T.length x - n) x
  , takeSubset  = \x i n -> T.take n (T.drop (i + 1) x)
  , findNewLine = T.findIndex (== '\n')
  }

{-------------------------------------------------------------------------------
   Within-group fold logic

A means of deciding whether include a group in the output
and returning a group's data unchanged if it is to be output.
A group is output if at least one of its elements
both successfully parses (given a parser) and
satisfies a given predicate.
Input elements that fail to parse are included in the output.
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
  | isEmpty fs x = status
  | otherwise =
    -- exit as soon as the predicate is satisfied
                if predicateSatified status
    then status
    else case lastNewLine status of
                                                   -- We're done as soon as there are no more new line characters.
      Nothing -> status
      -- If there was a new line character at the previous iteration,
      -- see if there is another new line character *after*
      -- the previous new line character.
      Just i  -> case findNewLine fs (drop' fs (i + 1) x) of
          -- If there is a previous and current new line character,
          -- then update the `GrpLines` tracker 
          -- with the value of the predicate from the current line
          -- and the index of the current new line character 
          -- (relative to whole input).
          -- Then continue.
        Just n -> go (takeSubset fs x i n) (Just $ i + n + 1)
        Nothing ->
        -- If there is a previous new line but no current new line,
        -- then update the `GrpLines` tracker and continue.
          go (takeEnd fs x i) Nothing
 where
  go i j = processGroupLinesInternal
    fs
    psl
    prd
    (MkGrpLines (parseThenPredicate psl prd i) j)
    x
{-# INLINE processGroupLinesInternal #-}

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

{-|
$processGroupLines

The @processGroupLines*@ functions take
all of a group's lines as single input.
The function identifies whether *any* line within the group
satisfy a predicate.
If so, then the group's lines are returned unchanged;
otherwise, an empty string is returned.

In the context of creating cohorts from event lines,
this operation is often useful when prefiltering events
in order to reduce the size of the input data.
For example, if the cohort is consists only of females,
one could run a prefilter to remove any groups (subjects in this case)
of event lines for males.

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
and mostly useful testing and demostration.

The following is a trivial example of the logic at work.
The data are newline delimited integers.
The parser simply parses the strings @"1"@ and @"2"@,
and the predicate checks whether the value is @1@.

>>> let allOnes = "1\n1\n1"
>>> let allTwos = "2\n2\n2"
>>> let i321 = "3\n2\n1"
>>> readOne x = if x == "1" || x == "2" then Just 1 else Nothing 
>>> processGroupLinesText readOne (==1) allOnes 
"1\n1\n1"
>>> processGroupLinesText readOne (==1) allTwos
""
>>> processGroupLinesText readOne (==1) i321

-}

processGroupLinesStrict
  :: (B.ByteString -> Maybe a) -> (a -> Bool) -> B.ByteString -> B.ByteString
processGroupLinesStrict = processGroupLines lineFunctionsStrict

processGroupLinesLazy
  :: (BL.ByteString -> Maybe a) -> (a -> Bool) -> BL.ByteString -> BL.ByteString
processGroupLinesLazy = processGroupLines lineFunctionsLazy

processGroupLinesText :: (T.Text -> Maybe a) -> (a -> Bool) -> T.Text -> T.Text
processGroupLinesText = processGroupLines lineFunctionsText

{-------------------------------------------------------------------------------
   Across-group fold ("application") logic

   This application processes streams of newline separated text at two levels:

(1) A means of grouping the stream. 
For example, if the input is event lines data,
the application groups the input events by subject ID. 
-------------------------------------------------------------------------------}

data OptionF i = MkOptionF
  { lastID       :: Maybe i
  , groupStart   :: Int
  , lastNewLine' :: Maybe Int
  , builder      :: Builder
  }

processAppLinesInternal
  :: (Eq i, Show i)
  => (BS.ByteString -> i)
  -> (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> OptionF i
  -> BS.ByteString
  -> OptionF i
processAppLinesInternal pri psl prd status x
  | BS.null x = status
  | otherwise = case lastNewLine' status of
      -- If no line end then done
    Nothing -> status
    -- Otherwise take the index of the newline character end as `i`
    Just i  ->
      -- Is there another newline after `i`?
               case BS.elemIndex '\n' (BS.drop (i + 1) x) of
        -- If yes, take this index as `n`,
        -- as in the count of characters from i to the next newline
      Just n ->
          -- Identify the groupID in the subset of x
          -- in the interval [i + 1, i + n].
          -- When the ID has not changed, 
          -- just update the index of the last newline.
          -- When the ID does change,
          -- then process the group for the last ID
          -- and update the ID in the accumulator
        let currentID = pri (takeLines i n)
        in
          if Just currentID == lastID status
            then go $ status { lastNewLine' = Just $ i + n + 1 }
            else go $ MkOptionF
              (Just currentID)
              (i + 1)
              (Just $ i + n + 1)
              (builder status <> byteString
                (processGroup
                  (takeLines (groupStart status - 1) (i - groupStart status + 1)
                  )
                )
              )
      Nothing ->
          -- The following is similar to the logic above,
          -- except it handles the case of 
          -- no further newlines to process.
        let currentID = pri (takeEnd i)
        in  if Just currentID == lastID status
              then go $ status { lastNewLine' = Nothing }
              else go $ MkOptionF
                (lastID status)
                i
                Nothing
                (  builder status
                <> byteString (processGroup (BS.drop (groupStart status) x))
                )

 where -- the recursion 
  go           = flip (processAppLinesInternal pri psl prd) x
  processGroup = processGroupLinesStrict psl prd
  takeEnd n = BS.drop (BS.length x - n) x
  takeLines i n = BS.take n (BS.drop (i + 1) x)
{-# INLINE processAppLinesInternal #-}


processAppLines
  :: (Eq i, Show i)
  => (BS.ByteString -> i)
  -> (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> BS.ByteString
  -> BS.ByteString
processAppLines pri psl prd x = BL.toStrict $ toLazyByteString $ builder
  (processAppLinesInternal pri
                           psl
                           prd
                           (MkOptionF Nothing (-1) (Just (-1)) mempty)
                           x
  )
