{-|
Core functionality for processing new line delimited data

In the context of creating cohorts from event lines,
the functionality herein may be useful when prefiltering events
in order to reduce the size of the input data.
For example, if the cohort is consists only of females,
one could run a prefilter to remove any groups (subjects in this case)
of event lines for males.
-}
{-# LANGUAGE DeriveGeneric #-}
module Hasklepias.AppBuilder.ProcessLines.Logic
  ( -- * Processing new line delimited data
    --
    -- $processAppLines
    processAppLinesStrict
  , processAppLinesLazy
  , LineProcessor(..)
  , LineStatus(..)
  ) where

import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Int
import           GHC.Generics

{-
-}
type LineAppMonad = Either LineAppError

{- 
INTERNAL
The type of errors in a line processing application
-}
data LineAppError =
    LineParseErrorA Int -- ^ indicates a failure of the @t -> Maybe a@ function
  | LineParseErrorID Int -- ^ indicates a failure of the @t -> Maybe id@ function
  deriving Eq

instance Show LineAppError where
  show (LineParseErrorA i) = "Line " <> show i <> ": failed to decode line"
  show (LineParseErrorID i) =
    "Line " <> show i <> ": failed to decode identifier"

{-
INTERNAL
Run a parser then a predicate, 
returning `Nothing` if the parsing failed.
-}
parseThenPredicate :: (t -> Maybe a) -> (a -> Bool) -> t -> Maybe (a, Bool)
parseThenPredicate psl prd x = fmap (fmap prd) $ (,) <$> psl x <*> psl x

{-
INTERNAL
Function that lifts a @Maybe (a, Bool)@ to the App monad.
The `Int` input is the line number passed in the case of an error.
-}
handleLineParse :: Int -> Maybe (a, Bool) -> LineAppMonad (a, Bool)
handleLineParse i x = case x of
  Nothing     -> Left $ LineParseErrorA i
  Just (x, b) -> Right (x, b)


{-
INTERNAL
A data type containing functions used in the line processing functions.
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
   Across-group fold ("application") logic

This application processes streams of newline delimited text at two levels:

(1) Groups of lines defined by a parser. 
For example, if the input is event lines data,
the application groups the input events by subject ID. 

(2) Processing each group

-------------------------------------------------------------------------------}

{- 
INTERNAL
Data tracking the state of the application.

NOTE: 
The `Maybe` of the `lastLineID` is used in two senses of `Nothing`:
* as the the "start" of processing
* as an error in the parsing of a group identifier from a line
-}
data AppLines id i = MkAppLines
  { lastLineID  :: Maybe id -- ^ the group ID at the previous line
  , grpStart    :: i -- ^  the index at which the current group started
  , grpStatus   :: Bool -- ^ the predicate status of a group
  , grpAcc      :: Maybe Builder -- ^ group-level accumulator
  , lastNewLine :: Maybe i -- ^ the index of the last new line
  , builderAcc  :: Builder -- ^ the accumulated results as a ByteString Builder
  , lineLog     :: AppLinesLog -- ^ a logger
  }

{-|
Log information about a line processing application.
-}
data AppLinesLog = MkAppLinesLog
  { groupsProcessed :: !Int -- ^ count of groups processed
  , groupsKept      :: !Int -- ^ count of groups dropped
  , linesProcessed  :: !Int -- ^ count of lines processed
  }
  deriving (Eq, Show, Generic)

-- INTERNAL 
incrementGroupCount :: AppLinesLog -> AppLinesLog
incrementGroupCount (MkAppLinesLog a b c) = MkAppLinesLog (a + 1) b c

-- INTERNAL 
incrementGroupKept :: AppLinesLog -> AppLinesLog
incrementGroupKept (MkAppLinesLog a b c) = MkAppLinesLog a (b + 1) c

{-
INTERNAL 
A type to avoid boolean blindness in processAppLinesInternal 
-}
data GroupChange = SameGroup | NewGroup

checkGroupChange :: (Eq id) => id -> id -> GroupChange
checkGroupChange x y = if x == y then SameGroup else NewGroup

{-|
This type's two functions are used in the @processAppLines*@ functions
to allow a developer to specify the logic of how to tranform lines.
The first function is the "transformer" which defines how to tranform the 
@a@ into the output @b@ type,
allowing to specify whether to drop or keep a line using the @`LineStatus`@ type.
In the case the line is to be kept,
the second function, the "builder",
creates the line to be output 
from the line identifer and the value of type @b@.
-}
data LineProcessor a b id = MkLineProcessor (a -> LineStatus b) -- ^ the transformer
                                            ((id, b) -> Builder) -- ^ the builder

{-|
A type used to indicate whether to drop or keep a line
in the transformer function of a @'LineProcessor'@.
-}
data LineStatus b =
      DropLine -- ^ indicates a line should be dropped from the output
    | KeepLine b  -- ^ includes a line should be kept in the output
  deriving (Eq, Show)

instance Functor LineStatus where
  fmap f DropLine     = DropLine
  fmap f (KeepLine x) = KeepLine (f x)

{-
INTERNAL
The core recursive logic of processing the lines across groups.
The function "rolls" over the *single string* of new line delimited data 
updating an `AppLines` value as it encounters new line characters
within that string.

IMPORTANT
All the lines for each group are assumed to be contiguous.

See @'processAppLinesStrict'@ for a description of arguments.

-}
processAppLinesInternal
  :: (Eq id, Show id, Num i, Monoid t, Show t)
  => LineFunctions t i
  -> (t -> Maybe id)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> Maybe (LineProcessor a b id)
  -> AppLines id i
  -> t
  -> LineAppMonad (AppLines id i)
processAppLinesInternal fs pri psl prd pro status x =
  case fmap (+ 1) (lastNewLine status) of
    -- If no new line then we're done!
    -- Simply update the accumulator for the last group.
    Nothing -> Right $ status
      { builderAcc = if grpStatus status
                       then updateAcc status Nothing
                       else builderAcc status
      , lineLog    = if grpStatus status
                       then incrementGroupKept (lineLog status)
                       else lineLog status
      }
    -- Otherwise take the index immediately after the newline character as `i`
    Just i -> do

      let getTail   = takeSubset fs x i
      -- Is there another newline character after `i`?
          nl        = findNewLine fs (getTail Nothing)
          thisLine  = getTail nl
          lineCount = linesProcessed (lineLog status) + 1
      -- always update the lastNewLine and count
          newStatus = status
            { lastNewLine = fmap (i +) nl
            , lineLog     = (lineLog status) { linesProcessed = lineCount }
            }

      if isEmpty fs thisLine
        then go newStatus

        -- When the ID has not changed, 
        -- just update the index of the last newline.
        -- When the ID does change,
        -- then process the group for the last ID
        -- and update the ID in the accumulator
        else case pri thisLine of
          Nothing         -> Left (LineParseErrorID lineCount)
          Just thisLineID -> do
            case
                ( checkGroupChange (Just thisLineID) (lastLineID status)
                , grpStatus status
                )
              of
              -- ID hasn't changed, predicate already satisfied ==> keep going
                (SameGroup, True) ->
                  checkLine lineCount thisLine
                    >>= (\(x, _) -> go newStatus
                          { grpAcc = updateGrp status x thisLineID
                          }
                        )
                -- ID hasn't changed, predicate not satisfied ==> 
                -- update status with this line
                (SameGroup, False) ->
                  checkLine lineCount thisLine
                    >>= (\(x, b) -> go newStatus
                          { grpStatus = b
                          , grpAcc    = updateGrp status x thisLineID
                          }
                        )
                -- ID has changed, predicate satisfied ==> 
                -- add last group to builder and update
                (NewGroup, True) ->
                  checkLine lineCount thisLine
                    >>= (\(x, b) -> go newStatus
                          { lastLineID = Just thisLineID
                          , grpStatus  = b
                          , grpStart   = i
                          , grpAcc     = toAcc $ processLine x thisLineID
                          , builderAcc = updateAcc status (Just i)
                          , lineLog = (incrementGroupKept . incrementGroupCount)
                                        (lineLog newStatus)
                          }
                        )

                -- ID has changed, predicate not satisfied ==> 
                -- reset group accumulator and update
                (NewGroup, False) ->
                  checkLine lineCount thisLine
                    >>= (\(x, b) -> go newStatus
                          { lastLineID = Just thisLineID
                          , grpStart   = i
                          , grpStatus  = b
                          , grpAcc     = toAcc $ processLine x thisLineID
                          , lineLog    = incrementGroupCount (lineLog newStatus)
                          }
                        )
 where
  go = flip (processAppLinesInternal fs pri psl prd pro) x
  checkLine i x = handleLineParse i $ parseThenPredicate psl prd x

  -- The function that processes a line depends on whether the user
  -- provided a line processor argument.
  processLine x y = case pro of
    Just (MkLineProcessor transformLine buildLine) ->
      fmap (\v -> buildLine (y, v)) (transformLine x)
    Nothing -> DropLine

  -- Cast a @LineStatus@ to a @Maybe@
  toAcc DropLine     = Nothing
  toAcc (KeepLine x) = Just x

  -- A helper function to update the group accumulator,
  -- whose logic depends on whether a LineProcessor is provided.
  updateGrp status line grpId = case pro of
    Just _ -> case processLine line grpId of
      -- keep going if line is to be dropped
      DropLine    -> grpAcc status
      -- handle the case that the accumulator may be @Nothing@
      -- and needs to be initialized
      KeepLine bu -> case grpAcc status of
        Nothing  -> Just bu
        Just acc -> Just (acc <> char8 '\n' <> bu)
    Nothing -> Nothing

  -- A helper function to update the main accumulator,
  -- whose logic depends on whether a LineProcessor is provided.
  updateAcc status i = case pro of
    Just _ -> case grpAcc status of
      Nothing  -> builderAcc status
      Just grp -> builderAcc status <> grp <> char8 '\n'
    Nothing -> builderAcc status <> build
      fs
      (takeSubset fs x (grpStart status) (fmap (\z -> z - grpStart status) i))
{-# INLINE processAppLinesInternal #-}

{-
INTERNAL
The function used to create a processAppLines* function
targeted for a specific type.

See @'processAppLinesStrict'@ for a description of arguments.
-}
processAppLines
  :: (Eq id, Show id, Num i, Monoid t, Show i, Show t)
  => LineFunctions t i
  -> (t -> Maybe id)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> Maybe (LineProcessor a b id)
  -> t
  -> LineAppMonad t
processAppLines fs pri psl prd pro x =
  let result = processAppLinesInternal
        fs
        pri
        psl
        prd
        pro
        (MkAppLines Nothing
                    0
                    False
                    mempty
                    (Just (-1))
                    mempty
                    (MkAppLinesLog 0 0 0)
        )
        x
  in  runBuilder fs . builderAcc <$> result
{-# INLINE processAppLines #-}

{- $processAppLines

The @processAppLines*@ functions conceptually splits a string 
into groups identified by an indentifier parsed from each line, 
and then check whether any line
within a group satisfies a predicate condition 
and (optionally) transforms and possibly drops each line.
When no @'LineProcessor'@ logic is supplied,
a single string is returned 
containing the lines from all groups
for which the predicate is satisfied.
If @'LineProcessor'@ logic is supplied,
then a single string is returned 
containing the lines from all groups
for which the predicate is satisfied 
AND those lines set to be kept by the @'LineProcessor'@ logic.

IMPORTANT: 
All the lines for each group should be contiguous in the input.

Two variants of @processAppLines*@ are available:

* 'processAppLinesStrict' works on strict 'BS.ByteString'. 
This version is generally faster, 
but requires that the entire input be loaded into memory.
* 'processAppLinesLazy' works on lazy 'BL.ByteString'.

These functions are difficult to demostrate succinctly.
For a complete example, 
see the source code in @Hasklepias.AppBuilder.ProcessLines.Tests@.

The logic of the @'LineProcessor'@ works in conjunction with the filtering logic
supplied as an argument.
For example, a group that has no line satisfying the given predicate
will have no output, 
even if the status of all the groups lines is @KeepLine@.
On the other hand, 
the logic of the tranformer function could specify to drop
lines that satisfy the predicate,
In short, developers can flexibly specify the logic of their application.

Related functions include:

* @'Hasklepias.AppBuilder.LineFilterApp.makeLineFilterApp'@: 
exposes the filtering logic of @'processAppLinesStrict'@ as an @IO ()@.

-}

-- | Process a strict 'BS.ByteString'.
processAppLinesStrict
  :: (Eq id, Show id)
  => (BS.ByteString -> Maybe id) -- ^ parser of a group identifier from a line
  -> (BS.ByteString -> Maybe a) -- ^ parser of an @a@ from a line
  -> (a -> Bool) -- ^ predicate to apply to each line
  -> Maybe (LineProcessor a b id) -- ^ an optional @'LineProcessor'@ 
  -> BS.ByteString -- ^ input string to be split into lines
  -> LineAppMonad BS.ByteString
processAppLinesStrict = processAppLines lineFunctionsStrict

-- | Process a lazy 'BL.ByteString'. 
-- See @'processAppLinesStrict'@ for a description of arguments.
processAppLinesLazy
  :: (Eq id, Show id)
  => (BL.ByteString -> Maybe id)
  -> (BL.ByteString -> Maybe a)
  -> (a -> Bool)
  -> Maybe (LineProcessor a b id)
  -> BL.ByteString
  -> LineAppMonad BL.ByteString
processAppLinesLazy = processAppLines lineFunctionsLazy
