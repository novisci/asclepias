module Main
  ( main
  ) where

import           Templates.Tests                ( templateTests )
import           Test.Tasty                     ( defaultMain )

main :: IO ()
main = defaultMain templateTests
