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

import Control.Applicative
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
import           Colog                          ( (<&)
                                                , (>$)
                                                , HasLog(..)
                                                , LogAction(..)
                                                , Message
                                                , WithLog
                                                , log
                                                , logError
                                                , logInfo
                                                , logPrint
                                                , logPrintStderr
                                                , logStringStderr
                                                , logStringStdout
                                                , logText
                                                , richMessageAction
                                                , withLog
                                                )
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


p :: Either String Silly -> Bool
p (Left _ ) = False
p (Right (Silly _ x)) = x < 6


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

finalState :: MyState -> B.ByteString
finalState (MyState (_ , b, x, acc)) = if b then acc <> "\n" <> x else acc

myConduitD =
       yield someSilliness2
    .| CC.linesUnboundedAscii
    .| mapC initMyState
    .| CC.foldl1 (<>)

main :: IO ()
main = do
    res <- runConduit myConduitD
    let out = fmap (B.toStrict . finalState) res
    case out of
      Nothing -> C.putStr ""
      Just x  -> C.putStrLn x
    -- C.putStrLn out

newtype MyState2 = MyState2 (Text, Bool, B.ByteString)
  deriving (Show)

instance Semigroup MyState2 where
  (<>) (MyState2 (id0, b0, x0)) (MyState2 (id1, b1, x1)) =
    if id0 == id1 then
        MyState2 (id1, getAny $ Any b0 <> Any b1, x0 <> "\n" <> x1)
    else
        MyState2 (id1, getAny $ Any b1, x1)

initMyState2 :: B.ByteString -> MyState2
initMyState2 x = MyState2 ( i , p l , x)
        where l = parseSillyLine x
              i = case l of
                Left s -> ""
                Right (Silly id int) -> id

getId :: MyState2 -> Text
getId (MyState2 (x, _, _)) = x

getBool :: MyState2 -> Bool
getBool (MyState2 (_, x, _)) = x

getAcc :: MyState2 -> B.ByteString
getAcc (MyState2 (_, _, x)) = x

ff :: MyState2 -> MyState2 -> IO MyState2
ff x y = do
        -- let errLog = logStringStderr
        z <- case (getId x /= getId y, getBool x) of
               (True, True)  -> C.putStrLn (B.toStrict $ getAcc x)
              --  (True, False) -> do errLog <& unpack (getId x)
               _             -> C.putStr ""
        return (x <> y)

fff :: IO MyState2 -> IO MyState2 -> IO MyState2
fff x y = do join (liftA2 ff x y)


myConduitE =
       yield someSilliness2
    .| CC.linesUnboundedAscii
    .| mapC (return . initMyState2)
    .| CC.foldl1 fff

main2 :: IO ()
main2 = do
    res <- runConduit myConduitE
    forM_ res ((C.putStrLn . (B.toStrict . getAcc)) =<<)