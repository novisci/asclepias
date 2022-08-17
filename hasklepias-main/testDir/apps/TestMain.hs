{-
Boilerplate code for running project tests
-}

import           Hasklepias
import           Tests

main :: IO ()
main = defaultMain (testGroup "P0000 Tests" [tests])