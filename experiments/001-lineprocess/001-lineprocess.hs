#!/usr/bin/env cabal
{- cabal:
build-depends:
    base ^>=4.14.3.0
    , aeson
    , acc
    , bytestring
    , conduit
    , containers
    , deepseq
    , foldl
    , string-interpolate
    , tasty
    , tasty-bench
    , tasty-hunit
    , text
    , vector
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{- HLINT ignore "Use camelCase" -}
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

module Main where

import           Acc
import           Conduit
import           Control.DeepSeq            (NFData, force)
import           Control.Exception          (evaluate)
import qualified Control.Foldl              as L
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Conduit.Combinators   as CC (filter, linesUnboundedAscii,
                                                   unlinesAscii)
import           Data.Conduit.List          as CL (groupBy)
import           Data.Maybe
import           Data.Sequence              hiding (fromList, null)
import           Data.String                (IsString)
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text)
import           Data.Vector                ((!))
import           Debug.Trace
import           GHC.Exts                   (IsList (fromList), IsString)
import           Test.Tasty
import           Test.Tasty.Bench
import           Test.Tasty.HUnit

-- INTERNAL
-- Run a parser then a predicate,
-- returning `False` if the parsing failed.
parseThenPredicate :: (t -> Maybe a) -> (a -> Bool) -> t -> Bool
parseThenPredicate psl prd = maybe False prd . psl

-- INTERNAL
-- Combine two strings, delimited by newline character.
unline :: (Semigroup t, IsString t) => t -> t -> t
unline x y = x <> "\n" <> y

{-------------------------------------------------------------------------------
   Within-group fold logic
-------------------------------------------------------------------------------}

{-
  Group Option A:
  Directly specify begin, step, done for a fold.
-}

type GroupState t = (Bool, t)

-- Fold step for within a group.
processGroupStep_OptionA
  :: (Semigroup t, IsString t, Eq t)
  => (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (GroupState t -> t -> GroupState t)
processGroupStep_OptionA prl prd (b, x) y = case (b, x == "") of
  (True , _    ) -> (b, unline x y)
  (False, True ) -> (parseThenPredicate prl prd y, y)
  (False, False) -> (parseThenPredicate prl prd y, unline x y)
{-# INLINE processGroupStep_OptionA #-}

-- Initial step of a group-level fold
processGroupBegin_OptionA :: IsString t => GroupState t
processGroupBegin_OptionA = (False, "")

-- Extraction step of a group-level fold
processGroupDone_OptionA :: IsString t => GroupState t -> t
processGroupDone_OptionA (b, x) = if b then x else ""

-- Defines the Control.Foldl version of the within group fold process.
processGroup_OptionA
  :: (Semigroup t, IsString t, Eq t)
  => (t -> Maybe a)
  -> (a -> Bool)
  -> L.Fold t t
processGroup_OptionA prl prd = L.Fold step begin done
 where
  begin = processGroupBegin_OptionA
  step  = processGroupStep_OptionA prl prd
  done  = processGroupDone_OptionA
{-# INLINE processGroup_OptionA #-}

{-
  Group Option B:
  Define a monoid from which `Control.Foldl.foldMap` constructs
  the appropriate fold logic.
-}

-- Just a wrapper in order to create a semigroup instance which adds a newline
newtype Lines t = MkLines { unLines :: t }

instance (IsString t, Semigroup t) => Semigroup (Lines t) where
  (<>) (MkLines x) (MkLines y) = MkLines (x <> "\n" <> y)
  {-# INLINE (<>) #-}

instance (IsString t, Semigroup t) => Monoid (Lines t) where
  mempty = MkLines ""

newtype GroupLines t = MkGroupLines (Bool, Lines t)
instance (IsString t, Semigroup t) => Semigroup (GroupLines t) where
  (<>) (MkGroupLines (a, xs)) (MkGroupLines (b, ys)) =
    MkGroupLines (a || b, xs <> ys)
  {-# INLINE (<>) #-}

instance (IsString t, Semigroup t) => Monoid (GroupLines t) where
  mempty = MkGroupLines (False, mempty)

toGroupLines :: (t -> Maybe a) -> (a -> Bool) -> t -> GroupLines t
toGroupLines psl prd x =
  let !line = x in MkGroupLines (parseThenPredicate psl prd x, MkLines line)
{-# INLINE toGroupLines #-}

getGroupLinesDone :: IsString t => GroupLines t -> t
getGroupLinesDone (MkGroupLines (b, MkLines x)) = if b then x else ""

getGroupLines :: IsString t => GroupLines t -> Lines t
getGroupLines (MkGroupLines (b, x)) = if b then x else MkLines ""

processGroup_OptionB
  :: (IsString t, Semigroup t)
  => (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool)
  -> L.Fold t t
processGroup_OptionB f h = L.foldMap (toGroupLines f h) getGroupLinesDone
{-# INLINE processGroup_OptionB #-}


{-
  Group Option C:

-}

data OptionC = MkOptionC
  { predicateSatified :: Bool
  , accumulated       :: Maybe BS.ByteString
  }
  deriving (Eq, Show)

processGroup_OptionCInternal
  :: (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> OptionC
  -> BS.ByteString
  -> OptionC
processGroup_OptionCInternal psl prd status x
  | BS.null x = status
  | otherwise = if predicateSatified status
    then MkOptionC True (unline (accumulated status) x)
    else case BS.elemIndex '\n' x of
                    -- If no new line, then you're at the last line.
      Nothing -> if parseThenPredicate psl prd x
        then MkOptionC True (unline (accumulated status) x)
        else MkOptionC False (Just BS.empty)
      -- Otherwise recursively process lines
      Just n -> processGroup_OptionCInternal
        psl
        prd
        (MkOptionC (parseThenPredicate psl prd (BS.take n x))
                   (unline (accumulated status) (BS.take n x))
        )
        (BS.drop (n + 1) x)
 where
  unline Nothing  y = Just y
  unline (Just x) y = Just (x <> "\n" <> y)
{-# INLINE processGroup_OptionCInternal #-}


processGroup_OptionC
  :: (BS.ByteString -> Maybe a) -> (a -> Bool) -> BS.ByteString -> BS.ByteString
processGroup_OptionC psl prd x = fromMaybe "" $ accumulated
  (processGroup_OptionCInternal psl prd (MkOptionC False Nothing) x)

{-
  Group Option D:

-}

data OptionD = MkOptionD
  { predicateSatified' :: Bool
  , lastIndex          :: Maybe Int
  }
  deriving (Eq, Show)

processGroup_OptionDInternal
  :: (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> OptionD
  -> BS.ByteString
  -> OptionD
processGroup_OptionDInternal psl prd status x
  | BS.null x = status
  | otherwise = if predicateSatified' status
    then status
    else case lastIndex status of
      Just i -> case endLine (BS.drop (i + 1) x) of
        Nothing -> go (MkOptionD (pp (takeEnd i)) Nothing)
        Just n  -> go (MkOptionD (pp (takeLine i n)) (Just $ i + n + 1))
      Nothing -> status
 where
  go      = flip (processGroup_OptionDInternal psl prd) x
  pp      = parseThenPredicate psl prd
  endLine = BS.elemIndex '\n'
  takeEnd n = BS.drop (BS.length x - n) x
  takeLine i n = BS.take n (BS.drop (i + 1) x)
{-# INLINE processGroup_OptionDInternal #-}


processGroup_OptionD
  :: (BS.ByteString -> Maybe a) -> (a -> Bool) -> BS.ByteString -> BS.ByteString
processGroup_OptionD psl prd x =
  let status = predicateSatified'
        $ processGroup_OptionDInternal psl prd (MkOptionD False (Just (-1))) x
  in  if status then x else BS.empty

{-------------------------------------------------------------------------------
   Across-group fold ("application") logic
-------------------------------------------------------------------------------}

{-
  Application Option A

  Line processing fold logic (non-monadic version)
  Defines the fold for a stream (across groups) of lines.
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
processLinesAppStep_OptionA
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (LinesAppState i t -> t -> LinesAppState i t)
processLinesAppStep_OptionA pid psl prd (prevId, x, acc) y = do
  let currId = pid y
  if prevId == currId
    then (prevId, processGroupStep_OptionA psl prd x y, acc)
    else
      let prevGroup = processGroupDone_OptionA x
      in  ( currId
          , (parseThenPredicate psl prd y, y)
          , if acc == "" then prevGroup else unline acc prevGroup
          )
{-# INLINE processLinesAppStep_OptionA #-}

-- Begin step for line process app
processLinesAppBegin_OptionA :: (IsString t) => (t -> i) -> LinesAppState i t
processLinesAppBegin_OptionA pid = (pid "", processGroupBegin_OptionA, "")

-- Done step for line process app
processLinesAppDone_OptionA
  :: (Semigroup t, IsString t) => LinesAppState i t -> t
processLinesAppDone_OptionA (_, lastGroup, x) =
  unline x (processGroupDone_OptionA lastGroup)

-- | `Control.Foldl.Fold` version of the line filter application logic.
processLinesApp_OptionA
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> L.Fold t t
processLinesApp_OptionA pid psl prd = L.Fold step begin done
 where
  step  = processLinesAppStep_OptionA pid psl prd
  begin = processLinesAppBegin_OptionA pid
  done  = processLinesAppDone_OptionA
{-# INLINE processLinesApp_OptionA #-}

-- Temp for testing
runProcessLinesApp_OptionA f g h =
  L.fold (processLinesApp_OptionA f g h) . BS.lines

{-
  Application Option B

  Line processing fold logic
  Defines the fold for a stream (across groups)
  for use in a context where IO can be done.

-}

-- (Current Group ID, GroupState)
-- This version does not have an accumulator,
-- as data may be output at each step.
type LinesAppStateM a t = (a, (Bool, t))

{-
A version of filterAppStep that outputs the previous group
when the identifier changes.
-}
processLinesAppStep_OptionB
  :: (Monad m, Eq i, Semigroup t, IsString t, Eq t)
  => (t -> m ()) -- ^ output function
  -> (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (LinesAppStateM i t -> t -> m (LinesAppStateM i t))
processLinesAppStep_OptionB put pid psl prd (i, x) y = do
  let newId = pid y
  if i == newId
    then pure (i, processGroupStep_OptionA psl prd x y)
    else
      (do
        processLineIO put x
        pure (newId, (parseThenPredicate psl prd y, y))
      )
{-# INLINE processLinesAppStep_OptionB #-}

processLineIO put (b, x) = if b then put x else pure ()

-- Begin step for line process app in monadic context
processLinesAppBegin_OptionB
  :: (Applicative m, IsString t) => (t -> i) -> m (i, GroupState t)
processLinesAppBegin_OptionB pid = pure (pid "", processGroupBegin_OptionA)

-- Done step for line process app in monadic context
processLinesAppDone_OptionB
  :: Applicative m => (t -> m ()) -> LinesAppStateM i t -> m ()
processLinesAppDone_OptionB put (i, x) = processLineIO put x

-- | `Control.Foldl.FoldM` version of the line processing application's logic.
processLinesApp_OptionB
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> IO ())
  -> (t -> i)
  -> (t -> Maybe a)
  -> (a -> Bool)
  -> L.FoldM IO t ()
processLinesApp_OptionB put pid psl prd = L.FoldM step begin done
 where
  step  = processLinesAppStep_OptionB put pid psl prd
  begin = processLinesAppBegin_OptionB pid
  done  = processLinesAppDone_OptionB put
{-# INLINE processLinesApp_OptionB #-}

{-
  App option C:
  Conduit-based filter application logic
  NOTE: this uses group option A for the within group fold
-}

-- | `Conduit` version of the line processing application's logic.
processLinesApp_OptionC
  :: (Eq i, Monad m)
  => (BS.ByteString -> i)  -- ^ identifier parser
  -> (BS.ByteString -> Maybe a)  -- ^ parsing function
  -> (a -> Bool)  -- ^ predicate
  -> BS.ByteString
  -> ConduitT a2 o m BS.ByteString
processLinesApp_OptionC pid psl prd x =
  yield x
    .| CC.linesUnboundedAscii
    .| CL.groupBy (\x y -> pid x == pid y)
    .| mapC (L.fold (processGroup_OptionA psl prd))
    .| CC.filter (/= "")
    .| CC.unlinesAscii
    .| foldC

-- TEMP for testing
runProcessLinesApp_OptionC f g h x =
  runConduitPure $ processLinesApp_OptionC f g h x


{-
  App Option D:
  Monoid approach for application fold
-}

newtype AppLines i t = MkAppLines (Maybe i, GroupLines t, t)

instance (IsString t, Semigroup t, Eq i) => Semigroup (AppLines i t) where
  (<>) (MkAppLines (a, xs, accx)) (MkAppLines (b, ys, accy)) = if a == b
    then MkAppLines (a, xs <> ys, accx <> accy)
    else MkAppLines (b, ys, accx <> getGroupLinesDone xs <> accy)
  {-# INLINE (<>) #-}

instance (IsString t, Semigroup t, Eq i) => Monoid (AppLines i t) where
  mempty = MkAppLines (Nothing, mempty, "")

toAppLines
  :: (IsString t)
  => (t -> i)
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool)
  -> t
  -> AppLines i t
toAppLines psi prl prd x =
  MkAppLines (Just $ psi x, toGroupLines prl prd x, "")
{-# INLINE toAppLines #-}

processLinesApp_OptionD
  :: (IsString t, Semigroup t, Eq i)
  => (t -> i)
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool)
  -> L.Fold t t
processLinesApp_OptionD psi prl prd = L.foldMap
  (toAppLines psi prl prd)
  (\(MkAppLines (_, x, acc)) -> acc <> getGroupLinesDone x)
{-# INLINE processLinesApp_OptionD #-}

{-
  App Option E:
  Similar to option D
  but directly writing fold logic,
  making use of parts of group option B
-}

processLinesAppStep_OptionE
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> (AppLines i t -> t -> AppLines i t)
processLinesAppStep_OptionE pid psl prd (MkAppLines (prevId, x, acc)) y = do
  let currId  = Just $ pid y
  let newLine = toGroupLines psl prd y
  if prevId == currId
    then MkAppLines (prevId, x <> newLine, acc)
    else
      let prevGroup = getGroupLinesDone x
      in  MkAppLines
            ( currId
            , newLine
            , if acc == "" then prevGroup else unline acc prevGroup
            )
{-# INLINE processLinesAppStep_OptionE #-}

-- Begin step for line process app
processLinesAppBegin_OptionE
  :: (IsString t, Semigroup t) => (t -> i) -> AppLines i t
processLinesAppBegin_OptionE pid = MkAppLines (Nothing, mempty, "")

-- Done step for line process app
processLinesAppDone_OptionE :: (Semigroup t, IsString t) => AppLines i t -> t
processLinesAppDone_OptionE (MkAppLines (_, lastGroup, x)) =
  unline x (getGroupLinesDone lastGroup)

-- | `Control.Foldl.Fold` version of the line filter application logic.
processLinesApp_OptionE
  :: (Eq i, Semigroup t, IsString t, Eq t)
  => (t -> i) -- ^ identifier parser
  -> (t -> Maybe a) -- ^ parsing function
  -> (a -> Bool) -- ^ predicate
  -> L.Fold t t
processLinesApp_OptionE pid psl prd = L.Fold step begin done
 where
  step  = processLinesAppStep_OptionE pid psl prd
  begin = processLinesAppBegin_OptionE pid
  done  = processLinesAppDone_OptionE
{-# INLINE processLinesApp_OptionE #-}

-- temp for testing
runProcessLinesApp_OptionE f g h =
  L.fold (processLinesApp_OptionE f g h) . BS.lines



{-
  App Option F:
  Applies group option D at the app level
-}

data OptionF i = MkOptionF
  { lastID      :: Maybe i
  , groupStart  :: Int
  , lastNewLine :: Maybe Int
  , builder     :: Builder
  }

processLinesApp_OptionFInternal
  :: (Eq i, Show i)
  => (BS.ByteString -> i)
  -> (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> OptionF i
  -> BS.ByteString
  -> OptionF i
processLinesApp_OptionFInternal pri psl prd status x
  | BS.null x = status
  | otherwise = case lastNewLine status of
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
            then go $ status { lastNewLine = Just $ i + n + 1 }
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
              then go $ status { lastNewLine = Nothing }
              else go $ MkOptionF
                (lastID status)
                i
                Nothing
                (  builder status
                <> byteString (processGroup (BS.drop (groupStart status) x))
                )

 where -- the recursion
  go           = flip (processLinesApp_OptionFInternal pri psl prd) x
  processGroup = processGroup_OptionD psl prd
  takeEnd n = BS.drop (BS.length x - n) x
  takeLines i n = BS.take n (BS.drop (i + 1) x)
{-# INLINE processLinesApp_OptionFInternal #-}


processLinesApp_OptionF
  :: (Eq i, Show i)
  => (BS.ByteString -> i)
  -> (BS.ByteString -> Maybe a)
  -> (a -> Bool)
  -> BS.ByteString
  -> BS.ByteString
processLinesApp_OptionF pri psl prd x =
  BL.toStrict $ toLazyByteString $ builder
    (processLinesApp_OptionFInternal
      pri
      psl
      prd
      (MkOptionF Nothing (-1) (Just (-1)) mempty)
      x
    )



{-
      Types for testing
-}

newtype LineAppTesterID = MkLineAppTesterID Int deriving (Show, Eq)

instance FromJSON LineAppTesterID where
  parseJSON = withArray "FooID" $ \a -> do
    id <- parseJSON (a ! 0)
    pure $ MkLineAppTesterID id

newtype LineAppTester = MkLineAppTester Bool deriving (Show, Eq, Ord)

instance FromJSON LineAppTester where
  parseJSON = withArray "Foo" $ \a -> do
    id <- parseJSON (a ! 1)
    pure $ MkLineAppTester id

dciS' = decodeStrict' @LineAppTesterID
dclS' = decodeStrict' @LineAppTester
dciS = decodeStrict @LineAppTesterID
dclS = decodeStrict @LineAppTester

dciL' = decode' @LineAppTesterID
dclL' = decode' @LineAppTester
dciL = decode @LineAppTesterID
dclL = decode @LineAppTester

tpr = (== MkLineAppTester True)


{-
      Test values constructors
-}

mkTestInput :: Int -> Text -> BS.ByteString
mkTestInput y x = [i|[#{ show y }, #{ x }]|]

mkTestInputL :: Int -> Text -> BL.ByteString
mkTestInputL y x = [i|[#{ show y }, #{ x }]|]

mkTestLines x = BS.intercalate "\n" (fmap (uncurry mkTestInput) x)


-- Strict bytestrings
passLine = flip mkTestInput "true"
failLine = flip mkTestInput "false"
badLine = flip mkTestInput "1"

mkLines n = BS.unlines . Prelude.replicate n
passLines n x = mkLines n (passLine x)
failLines n x = mkLines n (failLine x)
badLines n x = mkLines n (badLine x)

nFailOnepass n x = BS.concat [failLines n x, passLines 1 x]
onePassNfail n x = BS.concat [passLines 1 x, failLines n x]

mkGroupLines x = BS.concat $ Prelude.zipWith (\(f, i) -> f i) x [1 ..]

-- Lazy btyestrings

passLineL = flip mkTestInputL "true"
failLineL = flip mkTestInputL "false"
badLineL = flip mkTestInputL "1"

mkLinesL n = BL.unlines . Prelude.replicate n
passLinesL n x = mkLinesL n (passLineL x)
failLinesL n x = mkLinesL n (failLineL x)
badLinesL n x = mkLinesL n (badLineL x)

nFailOnepassL n x = BL.concat [failLinesL n x, passLinesL 1 x]
onePassNfailL n x = BL.concat [passLinesL 1 x, failLinesL n x]

mkGroupLinesL x = BL.concat $ Prelude.zipWith (\(f, i) -> f i) x [1 ..]


{-
      Benchmarks
-}

listInput :: Int -> [BS.ByteString]
listInput n =
  uncurry mkTestInput <$> Prelude.replicate n (1 :: Int, "0" :: Text)

makeBench
  :: (NFData a, NFData b) => (a -> b) -> String -> a -> String -> Benchmark
makeBench f fn i ipts =
  env (evaluate (force i)) $ \d -> bench (fn <> ":" <> ipts) $ nf f d

makeBenches
  :: (NFData a, NFData b) => a -> String -> [(a -> b, String)] -> [Benchmark]
makeBenches i ipts = fmap (\(a, b) -> makeBench a b i ipts)

makeGroupBenches
  :: (NFData a, NFData b, Num t1)
  => (p -> t1 -> t2)
  -> (t2 -> a)
  -> String
  -> p
  -> [(a -> b, String)]
  -> [Benchmark]
makeGroupBenches lineMaker lineSplitter inputlabel n x = do
  let inputs = lineSplitter $ lineMaker n 1
  makeBenches inputs inputlabel x

experimentInputsS
  :: (NFData a)
  => (BS.ByteString -> a)
  -> Int
  -> [[(a -> BS.ByteString, String)] -> [Benchmark]]
experimentInputsS lineSplitter n = fmap
  (\(a, b, c) -> makeGroupBenches a b c n)
  [ (failLines   , lineSplitter, "all fail :: " <> show n)
  , (passLines   , lineSplitter, "all pass :: " <> show n)
  , (onePassNfail, lineSplitter, "first pass :: " <> show n)
  , (nFailOnepass, lineSplitter, "last pass :: " <> show n)
  ]

experimentInputsL
  :: (NFData a)
  => (BL.ByteString -> a)
  -> Int
  -> [[(a -> BL.ByteString, String)] -> [Benchmark]]
experimentInputsL lineSplitter n = fmap
  (\(a, b, c) -> makeGroupBenches a b c n)
  [ (failLinesL   , lineSplitter, "all fail ::  " <> show n)
  , (passLinesL   , lineSplitter, "all pass :: " <> show n)
  , (onePassNfailL, lineSplitter, "first pass :: " <> show n)
  , (nFailOnepassL, lineSplitter, "last pass :: " <> show n)
  ]


processorsS :: Foldable f => [(f BS.ByteString -> BS.ByteString, String)]
processorsS =
  [ (L.fold (processGroup_OptionA dclS' tpr), "option A :: strict parse :: ")
  , (L.fold (processGroup_OptionB dclS' tpr), "option B :: strict parse :: ")
  -- , (L.fold (processGroup_OptionA dclS tpr) , "option A :: lazy parse :: ")
  -- , (L.fold (processGroup_OptionB dclS tpr) , "option B :: lazy parse :: ")
  ]


processorsL :: Foldable f => [(f BL.ByteString -> BL.ByteString, String)]
processorsL =
  [ (L.fold (processGroup_OptionA dclL' tpr), "option A :: strict parse :: ")
  , (L.fold (processGroup_OptionB dclL' tpr), "option B  :: strict parse :: ")
  , (L.fold (processGroup_OptionA dclL tpr) , "option A :: lazy parse :: ")
  , (L.fold (processGroup_OptionB dclL tpr) , "option B  :: lazy parse :: ")
  ]

linesAccS :: BS.ByteString -> Acc BS.ByteString
linesAccS = fromList . BS.lines

linesAccL :: BL.ByteString -> Acc BL.ByteString
linesAccL = fromList . BL.lines

linesSeqS :: BS.ByteString -> Seq BS.ByteString
linesSeqS = fromList . BS.lines

linesSeqL :: BL.ByteString -> Seq BL.ByteString
linesSeqL = fromList . BL.lines

runGroupExperiment1 :: Int -> [Benchmark]
runGroupExperiment1 n =
  fmap (\ex -> bgroup "strict bytestring :: list :: " $ ex processorsS)
       (experimentInputsS BS.lines n)
    ++ fmap
         (\ex -> bgroup "strict bytestring :: NA :: " $ ex
           [(processGroup_OptionC dclS' tpr, "option C :: strict parse ")]
         )
         (experimentInputsS id n)
    -- ++ fmap (\ex -> bgroup "lazy bytestring :: list :: " $ ex processorsL)
    --         (experimentInputsL BL.lines n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: acc :: " $ ex processorsS)
    --         (experimentInputsS linesAccS n)
    -- -- ++ fmap (\ex -> bgroup "lazy bytestring :: acc :: " $ ex processorsL)
    -- --         (experimentInputsL linesAccL n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: seq :: " $ ex processorsS)
    --         (experimentInputsS linesSeqS n)
    -- ++ fmap (\ex -> bgroup "lazy bytestring :: seq :: " $ ex processorsL)
    --         (experimentInputsL linesSeqL n)

runGroupExperiment2 :: Int -> [Benchmark]
runGroupExperiment2 n =
  fmap (\ex -> bgroup "strict bytestring :: list :: " $ ex processorsS)
       (experimentInputsS BS.lines n)
    ++ fmap
         (\ex -> bgroup "strict bytestring :: NA :: " $ ex
           [ (processGroup_OptionC dclS' tpr, "option C :: strict parse ")
           , (processGroup_OptionD dclS' tpr, "option D :: strict parse ")
           ]
         )
         (experimentInputsS id n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: acc :: " $ ex processorsS)
    --         (experimentInputsS linesAccS n)
    -- ++ fmap (\ex -> bgroup "strict bytestring :: seq :: " $ ex processorsS)
    --         (experimentInputsS linesSeqS n)


makeAppBenchInput f m n = mkGroupLines $ Prelude.replicate n (f, m)

makeAppBenchInputs =
  [ ("all-pass"  , makeAppBenchInput passLines)
  , ("all-fail"  , makeAppBenchInput failLines)
  , ("first-pass", makeAppBenchInput onePassNfail)
  , ("last-pass" , makeAppBenchInput nFailOnepass)
  ]

-- appBenchCounts =
--   [(100000, 10), (10000, 100), (1000, 1000), (100, 10000), (10, 100000)]

appBenchCounts = [(10000, 10), (1000, 100), (100, 1000), (10, 10000)]

-- appBenchCounts =
--     [
--       (1000, 10)
--     , (100, 100)
--     , (10, 1000)
--     , (1, 10000)
--     ]

cartProd x y = (,) <$> x <*> y

appBenchInputs :: [(String, BS.ByteString)]
appBenchInputs = fmap
  (\((m, n), (s, f)) -> (show n <> "groups-" <> show m <> "lines-" <> s, f m n))
  (cartProd appBenchCounts makeAppBenchInputs)

app_optionA = L.fold (processLinesApp_OptionA dciS' dclS' tpr)
-- app_optionB = L.foldM (processLinesApp_OptionB BS.putStr dciS' dclS' tpr)
app_optionC = runProcessLinesApp_OptionC dciS' dclS' tpr
app_optionD = L.fold (processLinesApp_OptionD dciS' dclS' tpr)
app_optionE = L.fold (processLinesApp_OptionE dciS' dclS' tpr)
app_optionF = processLinesApp_OptionF dciS' dclS' tpr

runAppExperiment1 = fmap
  (\((inputLabel, input), (fLabel, f)) -> makeBench f fLabel input inputLabel)
  (cartProd
    appBenchInputs
    [ ("app_optionA", app_optionA . BS.lines)
    -- , ("app_optionB", app_optionB . BS.lines)
    , ("app_optionC", app_optionC)
    , ("app_optionD", app_optionD . BS.lines)
    , ("app_optionE", app_optionE . BS.lines)
    , ("app_optionF", app_optionF)
    ]
  )



benches =
  bgroup
      "group experiments"
      (  Prelude.concatMap runGroupExperiment1 [10, 100, 1000]
      ++ Prelude.concatMap runGroupExperiment2 [10000, 100000]
      )
    :
      --  :
      --  runAppExperiment1
  --      :
      [bgroup "app experiments" runAppExperiment1]

-- These files were created in a ghci session for benchmarking externally,
-- using hyperfine.
-- file1 = BS.writeFile "lineFilter-test/100000groups-10lines.jsonl" (appBenchInputs 100000 10)
-- file2 = BS.writeFile "lineFilter-test/10000groups-100lines.jsonl" (appBenchInputs 10000 100)
-- file3 = BS.writeFile "lineFilter-test/1000groups-1000lines.jsonl" (appBenchInputs 1000 1000)
-- file4 = BS.writeFile "lineFilter-test/100groups-10000lines.jsonl" (appBenchInputs 100 10000)
-- file5 = BS.writeFile "lineFilter-test/10groups-100000lines.jsonl" (appBenchInputs 10 100000)

-- file1 = BS.writeFile "lineFilter-test/100000groups-10lines-allfail.jsonl" (appBenchInputsFail 100000 10)
-- file2 = BS.writeFile "lineFilter-test/10000groups-100lines-allfail.jsonl" (appBenchInputsFail 10000 100)
-- file3 = BS.writeFile "lineFilter-test/1000groups-1000lines-allfail.jsonl" (appBenchInputsFail 1000 1000)
-- file4 = BS.writeFile "lineFilter-test/100groups-10000lines-allfail.jsonl" (appBenchInputsFail 100 10000)
-- file5 = BS.writeFile "lineFilter-test/10groups-100000lines-allfail.jsonl" (appBenchInputsFail 10 100000)




{-
      Tests
-}

testCases =
  [ ("no input"      , ""           , "")
  , ("1 passing line", passLines 1 1, passLines 1 1)
  , ("1 failing line", failLines 1 1, "")
  , ("1 bad line"    , badLines 1 1 , "")
  , ( "1 group - with bad line"
    , BS.concat [passLines 1 1, badLines 1 1]
    , BS.concat [passLines 1 1, badLines 1 1]
    )
  , ( "1 group - 1 pass group"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "2 groups - 2 pass groups"
    , mkGroupLines [(nFailOnepass, 10)]
    , mkGroupLines [(nFailOnepass, 10)]
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(nFailOnepass, 10), (failLines, 10)]
    , nFailOnepass 10 1
    )
  , ( "2 groups - 1 pass group"
    , mkGroupLines [(failLines, 10), (nFailOnepass, 10)]
    , nFailOnepass 10 2
    )
  , ( "2 groups - 01 pass group"
    , mkGroupLines [(failLines, 10), (failLines, 10)]
    , ""
    )
  ]



tests = testGroup
  "filter lines application"
  [
    -- testGroup "conduit app" $ makeTests conduitApp testCases
    testGroup "option A" $ makeTests (app_optionA . BS.lines) testCases
  , testGroup "option E" $ makeTests (app_optionE . BS.lines) testCases
  ]
  where makeTests f = fmap (\(n, i, r) -> testCase n $ f i @?= r)


main :: IO ()
main = Test.Tasty.Bench.defaultMain benches
