{-|
TODO
-}
{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.AppBuilder.FilterApp.Streamly where 

import Hasklepias.AppBuilder.FilterApp.FilterLogic
import Data.String ( IsString )
import qualified Streamly.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Data.ByteString.Char8         as C
import Flow ( (|>) )
{-
Stream-based fold.
-}
groupS :: (Monad m, Semigroup a, IsString a, Eq a) =>
       (a -> Maybe b) -- ^ parser
    -> (b -> Bool) -- ^ predicate
    -> Fold.Fold m a a
groupS f g =
   fmap 
    filterGroupDone -- extract
    (Fold.foldl' 
      (filterGroupStep f g) -- step
       filterGroupInitial -- initial
    )

{-|
-}
filterGroupsS :: 
    (Stream.IsStream t, Monad m, Eq a, Semigroup b, IsString b, Eq b) =>
       (b -> a) 
    -> (b -> Maybe c) 
    -> (c -> Bool) 
    -> t m b
    -> t m b
filterGroupsS identifier parser predicate  = 
    Stream.groupsBy
      (\x y -> identifier x == identifier y)
      (groupS parser predicate)

{-|
-}
-- lines :: (Stream.IsStream t, Monad m, IsString a, Eq a) => 
--      t m a
--   -> t m [a]
-- lines = Stream.splitOnSuffix (== "\n") Fold.toList

{-|
-}
filterAppS :: Eq a =>
    (C.ByteString -> a)
    -> (C.ByteString -> Maybe c) 
    -> (c -> Bool) 
    -> IO ()
filterAppS f g h = 
     Stream.repeatM C.getLine
  |> filterGroupsS f g h
  |> Stream.mapM_ C.putStrLn

{-|
-}
filterAppSx :: Eq a =>
    (C.ByteString -> a)
    -> (C.ByteString -> Maybe c) 
    -> (c -> Bool) 
    -> C.ByteString
    -> [[C.ByteString]]
filterAppSx f g h x = 
     C.lines x
  |> Stream.fromList 
  |> filterGroupsS f g h
  |> Stream.intercalateSuffix (Unfold.function id) "\n"
  |> Stream.toList

{-|
-}
filterAppSy :: (Eq a) =>
    (C.ByteString -> a)
  -> (C.ByteString -> Maybe c)
  -> (c -> Bool)
  -> C.ByteString
  -> [C.ByteString]
filterAppSy f g h x = 
     C.lines x
  |> Stream.fromList
  |> Stream.fold (groupS g h)
  |> Stream.fold (Fold.foldl' unline "")




