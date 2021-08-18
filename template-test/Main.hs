module Main(
   main
) where

import Test.Tasty                 ( defaultMain )
import Hasklepias.Templates.Tests ( templateTests )

main :: IO ()
main =  defaultMain templateTests
