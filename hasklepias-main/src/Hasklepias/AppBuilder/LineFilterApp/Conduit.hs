module Hasklepias.AppBuilder.LineFilterApp.Conduit where

import Data.String
import Conduit
import Data.Conduit.Combinators as CC hiding (print)
import Data.Conduit.List as CL
import Hasklepias.AppBuilder.LineFilterApp.LineFilterLogic
import qualified Data.ByteString.Char8         as C
import qualified Control.Foldl as L
import Data.Conduit.Foldl


filterAppC :: (Eq a, Monad m) =>
       (C.ByteString -> m ())
  -> (C.ByteString -> a)
   -> (C.ByteString -> Maybe b)
   -> (b -> Bool)
   -> C.ByteString
   -> ConduitT a2 x m ()
filterAppC e f g h x =
    yield x
  .| CC.linesUnboundedAscii
  .| sinkFoldM (filterAppStepM e f g h) (filterAppBeginM f) (filterAppDoneM e)

foldGroups :: (Monad m, Eq b, IsString t) =>
  (t -> b)
  -> ConduitT t [t] m ()
foldGroups f = CL.groupBy (\x y -> f x == f y)

filterAppC' :: (Eq a, Monad m) =>
      (C.ByteString -> a)
   ->  (C.ByteString -> Maybe b)
   -> (b -> Bool)
   -> C.ByteString
   -> ConduitT a2 o m C.ByteString
filterAppC' f g h x =
    yield x
  .| CC.linesUnboundedAscii
  .| foldGroups f
  .| mapC (L.fold (filterGroupFold g h))
  .| CC.unlinesAscii
  .| foldC
