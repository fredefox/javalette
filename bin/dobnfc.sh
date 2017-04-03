#!/bin/bash
# Calls bnfc, moves some stuff around and cleans up after itself
bnfc Javalette.cf -p Javalette.Syntax -o src -m
cd src
make
rm Javalette/Syntax/LexJavalette.x
rm Javalette/Syntax/ParJavalette.y
mv Javalette/Syntax/TestJavalette.hs Javalette/Parser/Main.hs
rm Javalette/Syntax/TestJavalette
rm Makefile
sed -i '0,/Main/s//Javalette.Parser.Main/' Javalette/Parser/Main.hs
