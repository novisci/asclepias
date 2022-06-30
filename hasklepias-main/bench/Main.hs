module Main where 

import Test.Tasty.Bench
import Tests.MakeFilterApp

main :: IO ()
main = defaultMain benches