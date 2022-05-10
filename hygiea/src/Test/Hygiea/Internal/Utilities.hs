-- | Internal utilities. TODO: Some of these can be replaced with functionality from other packages.
module Test.Hygiea.Internal.Utilities where

import Data.Bifunctor

{- UTILITIES -}
maybeRight :: e -> Maybe a -> Either e a
maybeRight _   (Just x) = Right x
maybeRight err Nothing  = Left err

joinMaybeEither :: e -> Maybe (Either a b) -> Either e b
joinMaybeEither err (Just x) = first (const err) x
joinMaybeEither err Nothing  = Left err

-- outer error takes precedence
joinEitherOuter :: e -> Either e (Either a b) -> Either e b
joinEitherOuter err (Right x) = first (const err) x
joinEitherOuter _   (Left  x) = Left x

