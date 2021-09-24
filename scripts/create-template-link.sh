#!/bin/sh

cd hasklepias-templates/Templates/Features

file=$1
ln -s $file ${file%.*}.lhs

echo "symbol link created from ${file%.*}.lhs to $file"