module Main where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import System.Environment
main :: IO ()
main = 
  do 
    args <- getArgs
    pkg <- readGenericPackageDescription silent (args !! 1)
    putStrLn $ show $  packageName $ packageDescription pkg
    -- putStrLn packageName "Hello, Haskell!"
