{-|
  Defines the core types and aliases for the LineFilterApp tagging API.

  Projects using `asclepias` and its LineFilterApp should defined a
  `TaggerConfig t c m`, which provides a filepath to a dhall configuration file
  that can be marshaled to @M.Map Text c@, e.g. of the form `toMap { key1 =
  <val1>, key2 = <val2> }`, where the value types can be marshaled to type @c@
  via @FromDhall@.

  `TaggerConfig` also contains a list of `Tagger t c m`, functions that combine
  information from @M.Map Text c@ with values of type @m@ to produce a tag of
  type @t@. See @TaggerConfig@ for details.
      -}

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasklepias.AppBuilder.ProcessLines.Taggers where

import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import           Dhall
import           EventDataTheory (EventLine)



{- EXPORTED TYPES and CONFIG -}

-- | Entrypoint for specifying taggers in the `asclepias` line filter app.
-- Configuration required to read in a `dhall` file, @tagMapFile@, as a @M.Map
-- Text c@ for use in each of the @Tagger t c m@ functions. @t@ is the tag type
-- returned, and @m@ is the type with which values in the @M.Map Text c@
-- argument of the tagger will be compared to produce the tag. When
-- @TaggerConfig t c m@ is used in the `asclepias` line fitltering app, @t, m@
-- will correspond to @Event t m a@. 
--
-- @c@ will almost invariably be a @Codelist@ type, as exported by the
-- non-public `fact-models` package, and similarly the @m@ will be a model type
-- from that package.  Nonetheless they are left generic because the @Codelist@
-- and model types are not and will not be imported into `hasklepias-main`.
-- Note that @c@ is required to be @FromDhall@. The `dhall` shape from which
-- @M.Map Text c@ is marshaled is of the form `toMap { key1 = val1, key2
-- = val2, ... }`, where `val1, val2` are values that marshal to the Haskell
-- type `c`. 
--
-- The path `tagMapFile` should be relative to where the line filter app will
-- execute.
--
-- This example (not run in doctests) assumes you have a P0000Model in the
-- `fact-models` package that has a simple `Diagnosis` variant, which holds a
-- [`Code`](https://docs.novisci.com/event-data/3.0/models/facts.html#_code)
-- fact in its `code` field.
--
--  import Models.P0000Model
--  import qualified Data.Map.Strict as M
--  isCancer :: M.Map Text Codelist -> P0000Model -> Maybe Text
--  isCancer cls (Diagnosis cd) = makeTag =<< lookInCl cd.code . (\cl -> cl.sets) <$> SM.lookup "cancer" cls
--   where lookInCl cd' csets' = any (\cs -> cd'.codebook == cs.codebook  && (cd'.code `elem` Prelude.map (\x -> x.code) cs.codes)) csets'
--         makeTag b = if b then Just "is_cancer" else Nothing
--
--  tc :: TaggerConfig Text Codelist P0000Model
--  tc = MkTaggerConfig [isCancer] "plans/codelists.dhall"

data TaggerConfig t c m where 
  MkTaggerConfig :: (FromDhall c) => { taggers :: [Tagger t c m], tagMapFile :: FilePath } -> TaggerConfig t c m

-- TODO delete. exists only as a placeholder for runFilterEventLineAppSimple
-- until implementation MR is complete.
-- https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/331
emptyTaggerConfig :: TaggerConfig t Text m
emptyTaggerConfig = MkTaggerConfig [] ""

-- | Type alias for a tagging function, as used by the line filter application.
-- @c@ is the value type for a @M.Map Text c@, @m@ is a type to which those
-- values can be compared to (@Maybe@) produce some tag of type @t@. @t, m@
-- should correspond to @EventLine t m a@. A return of @Nothing@ will indicate
-- that no tag should be created. The return type is useful to accommodate
-- possible failed lookups in the @M.Map@.
type Tagger t c m = M.Map Text c -> m -> Maybe t

-- | Alias for a function that ingests a @Tagger c m@ and modifies an
-- @EventLine t m a@.
type EventLineTagger t c m a = Tagger t c m -> EventLine t m a -> EventLine t m a

  {- DHALL UTILITIES -}

-- TODO might want to change depending on
-- https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/331
-- That MR also should provide a more detailed implementation here, one like
-- dhall's 'inputWithSettings', which instead of calling throwIO in case of a
-- decoding error it provides a more helpful error message in this context,
-- about what shape the input file should be. We could alternatively use some
-- ugly try/catch statements in the runner itself, provide info to the logger,
-- etc.
-- | Construct the @M.Map Text c@ by parsing the @tagMapFile@.
inputTagMap :: TaggerConfig t c m -> IO (M.Map Text c)
inputTagMap MkTaggerConfig { tagMapFile  = tm } = inputFile auto tm
