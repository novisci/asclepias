module Main(
   main
) where

import Test.Tasty                 ( defaultMain )
import Templates.Tests ( templateTests )

main :: IO ()
main =  defaultMain templateTests
