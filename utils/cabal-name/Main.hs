module Main where

import Distribution.Package
import Distribution.Pretty
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Options.Applicative
import System.Environment
-- import qualified Text.PrettyPrint as PP

newtype CabalFile = MkCabalFile String

nameApp :: Parser CabalFile
nameApp = MkCabalFile <$> argument str (metavar "FILE")

opts :: ParserInfo CabalFile
opts = Options.Applicative.info
  (nameApp <**> helper)
  (progDesc "" <> header "cohort collector")

f :: CabalFile -> IO GenericPackageDescription
f (MkCabalFile x ) = readGenericPackageDescription silent x

main :: IO ()
main = 
  do 
    args <- execParser opts
    pkg <- f args
    -- putStrLn $ unPackageName $ packageName $ packageDescription pkg
    print (pretty $  packageName $ packageDescription pkg )
    -- print (pretty $  packageVersion $ packageDescription pkg )