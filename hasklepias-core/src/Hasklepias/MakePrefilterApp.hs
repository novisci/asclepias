{-|
Module      : Hasklepias.MakeApp
Description : Functions for creating a cohort application
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Hasklepias.MakePrefilterApp
  (
  ) where

import Data.Text
import GHC.Int
import GHC.Generics
import Data.Aeson as A
import Data.Bifunctor
import Data.Monoid
import Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.State
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8         as C
                                                -- ( lines )
data Silly = Silly Text Int
  deriving (Eq, Show, Generic)

instance ToJSON Silly
instance FromJSON Silly

type Status = Bool
type PrefilterState = (Bool, [B.ByteString])


someSilliness :: [B.ByteString]
someSilliness =
  ["[\"a\", 5]"
  ,"[\"a\", 6]"
  ,"[\"a\", 2]"
  ,"[\"b\", 2]"
  ,"[\"c\", 8]"
  ,"[\"c\", 8]"
  ,"[\"c\", 10]"
  ,"[\"d\", 4]"
  ,"[\"d\", 8]"
  ,"[\"e\", 1]"
  ]

someSilliness2 :: B.ByteString
someSilliness2 = B.intercalate "\n" someSilliness

parseSillyLine :: B.ByteString -> Either String Silly
parseSillyLine = eitherDecode

f :: B.ByteString -> Either String (Silly, B.ByteString)
f x = second (,x) (parseSillyLine x)

sillyList :: B.ByteString -> [Either String (Silly, B.ByteString)]
sillyList l = fmap (f . B.fromStrict) (C.lines $ B.toStrict l)

p :: Either String Silly -> Bool
p (Left _ ) = False
p (Right (Silly _ x)) = x < 6

doThing :: [B.ByteString] -> State PrefilterState Status
doThing [] = do
  (_, _) <- get
  return False

doThing (x:xs) = do
  (s, acc) <- get
  if p (parseSillyLine x) then
    put (True, acc ++ [x])
  else
    put (False, acc)
  doThing xs
  -- return xs

startThing :: (Bool, [B.ByteString] )
startThing = (False, [""])

-- test :: Status
test :: PrefilterState
test = execState (doThing someSilliness) startThing

{-
lifted from https://hackage.haskell.org/package/ndjson-conduit-0.1.0.1/docs/src/Data-Conduit-JSON-NewlineDelimited.html#serializer 
-}
-- carriage :: B.Bytestring
-- carriage = "\r"

-- newLine
-- newline = fromIntegral $ fromEnum '\n'
newtype MyState = MyState (Maybe Text, Bool, B.ByteString, B.ByteString)
  deriving (Show)

instance Semigroup MyState where
  (<>) (MyState (id0, b0, x0, acc0)) (MyState (id1, b1, x1, acc1)) = 
    if id0 == id1 then
        MyState (id1, getAny $ Any b0 <> Any b1, x0 <> "\n" <> x1, acc0)
    else
        MyState (id1, getAny $ Any b1, x1, 
                if b0 then acc0 <> "\n" <> x0 else acc0) 

-- instance Monoid MyState where
--   mempty = MyState (mempty, getAny mempty, mempty, mempty)

initMyState :: B.ByteString -> MyState
initMyState x = MyState ( i , p l , x , "")
        where l = parseSillyLine x
              i = case l of 
                Left s -> Nothing
                Right (Silly id int) -> Just id

              

-- | Consumes a stream of serializable values, and provides a stream of bytestrings.
-- serializer :: (Monad m, A.ToJSON a) => Conduit a m B.ByteString
-- serializer = mapInput A.toJSON (const Nothing) valueSerializer

-- | Consumes a stream of aeson values, and provides a stream of bytestrings.
-- valueSerializer :: Monad m => Conduit A.Value m B.ByteString
-- valueSerializer = await' $ (>> valueSerializer) . ( yieldBuilder . _ 
--   where yieldBuilder = mapM_ yield . B.toChunks . B.toLazyByteString
--         build a = A.encode a <> "\r" <> "\n"

-- myConduit :: (Monad m) => B.ByteString -> ConduitT B.ByteString B.ByteString m ()
-- myConduit :: B.ByteString -> ConduitM a c ((->) [B.ByteString]) (State PrefilterState Status)
myConduitA =
       yieldMany someSilliness
    .| mapC (\x-> decode x :: Maybe Array)
    .| mapM_C print

-- myConduitB :: ConduitM B.ByteString (Either String Silly) IO ()
myConduitB =
       yield someSilliness2
    .| CC.linesUnboundedAscii
    .| mapC parseSillyLine
    .| mapM_C print



myConduitC :: (Monad m) => ConduitM a c m Bool
myConduitC =
       yield someSilliness2
    .| CC.linesUnboundedAscii
    .| CC.any (p . parseSillyLine)

-- myConduitC :: ConduitM a (Either String Silly) IO ()
-- myConduitC :: (Monad m) => ConduitM a MyState m MyState
-- myConduitC =
--        yield someSilliness2
--     .| peekForeverE
--       ( do CC.lineAscii (mapC (\x -> MyState (p $ parseSillyLine x, x) )))
--     .| foldC
    -- .| CC.any (p)
    -- .| do (\(MyState (b, v)) -> if b then v else mempty)

-- myConduitD :: Monad m => ConduitT i B.ByteString m ()
-- myConduitD :: ConduitM a B.ByteString Identity ()
-- myConduitD :: (Monad m) => ConduitM a c m MyState
myConduitD =
       yield someSilliness2
    .| CC.linesUnboundedAscii
    .| mapC initMyState
    .| CC.foldl1 (<>) 
    -- .| CC.linesUnboundedAscii 
    -- .| peekForeverE ( do CC.lineAscii (mapC parseSillyLine))
    -- .| CC.any p

ff :: Monad m => B.ByteString -> m B.ByteString
ff x = do
        b <- runConduit $
                yield x
              .| CC.linesUnboundedAscii
              .| CC.any (p . parseSillyLine)
        return $ if b then x else mempty

-- sink :: Monad m => ConduitT B.ByteString B.ByteString m ()
sink :: Monad m => ConduitT B.ByteString B.ByteString m ()
sink = do
    x <- CC.linesUnboundedAscii
      .| CC.any (p . parseSillyLine)
    -- z <- x .| CC.unlinesAscii 
    return $ if x then mempty else mempty
    -- return (x, y)

-- main :: IO ()
main :: IO ()
main = do 
    res <- runConduit myConduitD
    -- res <- ff someSilliness2
    print res
  -- do
    -- res <- runConduit $ myConduitD  .| sink
    -- -- .| foldC
    --     .| mapM_C print
    -- print res

-- myConduitD :: (Monad m) => ConduitM MyState o m ()
-- myConduitD = foldC 


-- myConduitZ = 
--   myConduitC
--   -- .| myConduitD
--   .| mapM_C print
    -- .| mapM_C print
    -- .| stdoutC
    -- .| mapC parseSillyLine
    -- .| mapM_C print
    -- .| decodeUtf8
    -- .| peekForeverE
    --   (do 

    --       thing <- lineC (mapC ( parseSillyLine . B.build ))
    --         -- mapC parseSillyLine
    --       -- len <- lineC lengthCE 
    --       liftIO $ print thing)
    -- .| mapC decode
    -- .| mapM_C print

  -- -- do 
  --   sourceFile "blah.txt" .| decodeUtf8C
  --   .| do 
  --      len <- lineC lengthCE
  --      liftIO $ print len
    -- linesUnboundedC
    -- .| mapC C.toStrict
    -- .| mapC eitherDecode 
      -- yieldMany ( (C.lines . B.toStrict) x )
    -- .| mapC B.fromStrict
    -- .| lift doThing
    --  startThing


-- tryThis x = runConduit $ myConduit x 
