#!/usr/bin/env bash

testsec(){
cabal --enable-nix run SECD2 -- -o a $1 && bin/sec  < a 
}

make  sec 
echo "-----------------TEST 1------------------"
cabal --enable-nix run SECD2 -- -o a "(15+12)*3"&& bin/sec  < a 
echo "expected 81"
echo "-----------------TEST 2------------------"
cabal --enable-nix run SECD2 -- -o a "(\x.ifzero 1 2 ((\x.x) 10)) 3" && bin/sec  < a 
echo "expected 10"
echo "-----------------TEST 3------------------"
cabal --enable-nix run SECD2 -- -o a "(\x.x+1) 4" && bin/sec  < a 
echo "expected 5"
echo "-----------------TEST 4------------------"
cabal --enable-nix run SECD2 -- -o a "let e = 4 in e+1" && bin/sec  < a 
echo "expected 5"
echo "-----------------TEST 5------------------"
cabal --enable-nix run SECD2 -- -o a "(fix \f.\x.ifzero x 1 ( f (x-1))) 10" && bin/sec  < a 
echo "expected 1"
