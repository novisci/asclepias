module Main where

import           Build_doctests            (flags, module_sources, pkgs)
import           Data.Foldable             (traverse_)
import           System.Environment.Compat (unsetEnv)
import           Test.DocTest              (doctest)

{-
See https://github.com/haskellari/cabal-doctest/tree/master/multiple-components-example
for all details on how to set up doctests.
-}

main :: IO ()
main = do
  traverse_ putStrLn args
  unsetEnv "GHC_ENVIRONMENT"
  doctest args
  where args = flags ++ pkgs ++ module_sources
