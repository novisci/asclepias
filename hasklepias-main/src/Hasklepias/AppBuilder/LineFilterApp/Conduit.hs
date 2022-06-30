module Hasklepias.AppBuilder.LineFilterApp.Conduit where

import           Conduit
import qualified Control.Foldl                 as L
import qualified Data.ByteString.Char8         as C
import           Data.Conduit.Combinators      as CC
import           Data.Conduit.Foldl
import           Data.Conduit.List             as CL
import           Data.String
import           Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic

foldGroups :: (Monad m, Eq b, IsString t) => (t -> b) -> ConduitT t [t] m ()
foldGroups f = CL.groupBy (\x y -> f x == f y)

filterAppC
  :: (Eq a, Monad m)
  => (C.ByteString -> a)
  -> (C.ByteString -> Maybe b)
  -> (b -> Bool)
  -> C.ByteString
  -> ConduitT a2 o m C.ByteString
filterAppC f g h x =
  yield x
    .| CC.linesUnboundedAscii
    .| foldGroups f
    .| mapC (L.fold (filterGroupFold g h))
    .| CC.filter (/= "")
    .| CC.unlinesAscii
    .| foldC
