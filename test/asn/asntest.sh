#!/bin/bash

rm -rf out
mkdir -p out
asn1scc -o out -equal -c $1.asn
# copy the fixed version of the asn1 runtime header
# (contains type defintiions that c2nim fails to convert)
cp asn1crt.nim out
# copy the test case
cp asn.nim out
cd out
# Generate the bindings to give access to the ASN.1 types in nim
c2nim --importc $1.h
gcc -c *.c
nim c -l:*.o asn
