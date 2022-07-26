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
  ( -- * Processing multiple groups
    --
    -- $processAppLines
    processAppLinesStrict
  , processAppLinesLazy
  , LineProcessor(..)
  ) where

import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Int
import           Data.Maybe
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
  , linelog     :: AppLinesLog
  }

data AppLinesLog = MkAppLinesLog
  { groupsProcessed :: !Int -- ^ count of groups processed
  , groupsKept      :: !Int -- ^ count of groups dropped
  , linesProcessed  :: !Int -- ^ count of lines processed
  }
  deriving (Eq, Show, Generic)

incrementGroupCount :: AppLinesLog -> AppLinesLog
incrementGroupCount (MkAppLinesLog a b c) = MkAppLinesLog (a + 1) b c

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
TODO
-}
data LineProcessor a b id = MkLineProcessor (a -> Maybe b) ((id, b) -> Builder)


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
      , linelog    = if grpStatus status
                       then incrementGroupKept (linelog status)
                       else linelog status
      }
    -- Otherwise take the index immediately after the newline character as `i`
    Just i -> do

      let getTail    = takeSubset fs x i
      -- Is there another newline character after `i`?
          nl         = findNewLine fs (getTail Nothing)
          thisLine   = getTail nl
          lineCount = linesProcessed (linelog status) + 1
      -- always update the lastNewLine and count
          newStatus  = status
            { lastNewLine = fmap (i +) nl
            , linelog     = (linelog status) { linesProcessed = lineCount }
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
                          , grpStatus = b
                          , grpStart = i
                          , grpAcc = processLine <$> Just x <*> Just thisLineID
                          , builderAcc = updateAcc status (Just i)
                          , linelog = (incrementGroupKept . incrementGroupCount)
                                        (linelog newStatus)
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
                          , grpAcc = processLine <$> Just x <*> Just thisLineID
                          , linelog    = incrementGroupCount (linelog newStatus)
                          }
                        )
 where
  go = flip (processAppLinesInternal fs pri psl prd pro) x
  checkLine i x = handleLineParse i $ parseThenPredicate psl prd x
  processLine x y = case pro of
    Just (MkLineProcessor transformLine buildLine) ->
      maybe mempty (\v -> buildLine (y, v)) (transformLine x)
    Nothing -> mempty
  updateGrp status x y = case pro of
    Just p  -> (<> char8 '\n' <> processLine x y) <$> grpAcc status
    Nothing -> Nothing
  updateAcc status i = case pro of
    Just _ ->
      builderAcc status <> fromMaybe mempty (grpAcc status) <> char8 '\n'
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
within a group satisfies a predicate condition.
A single string is returned 
containing the lines from all groups
for which the predicate is satisfied.

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

-}

-- | Process a strict 'BS.ByteString'.
processAppLinesStrict
  :: (Eq id, Show id)
  => (BS.ByteString -> Maybe id) -- ^ parser of a group identifier from a line
  -> (BS.ByteString -> Maybe a) -- ^ parser of an @a@ from a line
  -> (a -> Bool) -- ^ predicate to apply to each line
  -> Maybe (LineProcessor a b id)
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
