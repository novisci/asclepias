#!/bin/bash
#
# a rudimentary script to initialize an asclepias project 

if [  $# -ne 3 ]
  then
    echo "3 arguments must be supplied"
fi

PROJID="$1"
HASKLEPIAS_VERS="$2"
FACTMODELS_VERS="$3"

cat << EOF
================================================================================
            AAA                lll                iii               
          AAAAA   sss    cccc lll   eee  pp pp         aa aa  sss  
          AA   AA s     cc     lll ee   e ppp  pp iii  aa aaa s     
          AAAAAAA  sss  cc     lll eeeee  pppppp  iii aa  aaa  sss  
          AA   AA     s  ccccc lll  eeeee pp      iii  aaa aa     s 
                  sss                    pp                   sss 
================================================================================           
EOF

# It would be nice to simply run `cabal init`,
# but doesn't quite create the structure we want.
# For example, it creates `MyLib.hs` as the library,
# whereas we want the project name.
# This means we have to run cabal init and then modify stuff.
# For now, I've taken the approach of just create exactly what we want
# in this script.
# The command below gets pretty close, 
# so I'm leaving it here for future reference.
# cabal init \
#   --libandexe \
#   --tests \
#   --test-dir=tests \
#   --application-dir=apps \
#   --source-dir=plans \
#   --package-name="$PROJID" \
#   --minimal \
#   --no-comments \
#   --homepage=https://gitlab.novisci.com/nsResearch/"$PROJID" \
#   --dependency=hasklepias

# Create the directory structure
mkdir apps
mkdir plans
mkdir tests

# Create initial files
cat << EOF > "$PROJID".cabal
cabal-version:      2.4
name:               $PROJID
version:            0.1.0
homepage:           https://gitlab.novisci.com/nsResearch/$PROJID
extra-source-files:
    README.adoc

library
    exposed-modules:
         ${PROJID}
       , Tests 
    build-depends:    
        hasklepias-main ^>= ${HASKLEPIAS_VERS}
      , fact-models ^>= ${FACTMODELS_VERS}
    hs-source-dirs:   plans
    default-language: Haskell2010
    default-extensions: 
        NoImplicitPrelude
        OverloadedStrings
        LambdaCase
        FlexibleContexts
        FlexibleInstances
        DeriveGeneric
        DuplicateRecordFields
        MultiParamTypeClasses
        TupleSections

executable $PROJID
    main-is:          Main.hs
    build-depends:
        base,
        hasklepias-main,
        $PROJID
    hs-source-dirs:   apps
    default-language: Haskell2010

test-suite ${PROJID}-test
    default-language: Haskell2010
    type:             exitcode-stdio-1.0
    hs-source-dirs:   tests
    main-is:          Main.hs
    build-depends:    base, hasklepias-main, ${PROJID}
EOF

cat << EOF > cabal.project
packages: ./.

source-repository-package
  type: git
  location: https://gitlab+deploy-token-ns-projects:glpat-LCmSwKeZSLheZCTVMEkA@gitlab.novisci.com/nsStat/asclepias.git
  tag: e70d889bd23f3e598fbc85ef64f684fbdd99ce23
  subdir: 
    hasklepias-appBuilder
    hasklepias-core
    hasklepias-main
    hasklepias-templates
    event-data-theory
    stype

source-repository-package
   type: git
   location: https://gitlab+deploy-token-ns-projects:glpat-7Z4w2JGrm2692Bshcqhd@gitlab.novisci.com/nsStat/event-data-model-new.git
   tag: f62b1fd9a26e8d559d803cae6d9c61bb409438f9
   subdir: 
    fact-models

allow-older:
   fact-models:generic-lens,
   fact-models:tasty
EOF

cat << EOF > plans/"${PROJID}".hs
{-|
Starting point for $PROJID cohort development
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module ${PROJID} where

import Hasklepias


EOF

cat << EOF > plans/Tests.hs 
{-|
Starting point for $PROJID testing 
-}
module Tests where 

import Hasklepias
import ${PROJID}

planTests :: TestTree
planTests  = testGroup 
    "TODO: implement tests" 
    [ testCase "trivial" $ True @?= True ]
EOF

cat << EOF > apps/Main.hs
{-|
Starting point for $PROJID application
-}
module Main where

import qualified ${PROJID}

main :: IO ()
main = do -- replace with result of makeCohortApp when ready
  putStrLn "Hello, $PROJID!"
  --- ${PROJID}.cohortApp

EOF

cat << EOF > tests/Main.hs
{-
A simple script for running tests
-}
import Hasklepias
import Tests 

main :: IO ()
main = defaultMain (testGroup "$PROJID Tests" [ planTests ])
EOF

cat << EOF >> .gitignore
# Haskell build artifacts
*dist-newstyle

# Dhall artifacts
.history
.dhallb

# nix artifacts
*result

# R stuff
.Rproj.user
.Rhistory
.RData
.Ruserdata

# drake (R package) artifacts
.drake*
.drake_history*

# IDE settings
*.code-workspace
*.Rproj
.vscode*

# etc
*DS_Store
EOF

echo -e "\n"
echo "Created project structure for ${PROJID} "
echo "Try running:" 
echo "   cabal update && cabal build" 
echo "Note first build can take awhile!"
echo -e "\n"
