{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.AppBuilder.LineFilterApp.Foldl where

import qualified Control.Foldl as L
import Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
import Data.List
import Flow
import qualified Data.ByteString.Char8         as C


filterAppF :: (Eq i, Eq a) =>
     (C.ByteString -> i)
  -> (C.ByteString -> Maybe a)
  -> (a -> Bool)
  -> C.ByteString
  -> IO ()
filterAppF f g h x = 
     C.lines x
  |> L.foldM (filterAppFoldM C.putStrLn f g h)
  

filterAppF' :: (Eq i, Eq a) =>
     (C.ByteString -> i)
  -> (C.ByteString -> Maybe a)
  -> (a -> Bool)
  -> C.ByteString
  -> IO ()
filterAppF'  f g h x = 
     C.lines x
  |> groupBy  (\x y -> f x == f y)
  |> fmap (L.fold (filterGroupFold g h))
  |> C.unlines
  |> C.putStrLn 
 