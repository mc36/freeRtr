#!/bin/sh
echo compiling
rm -rf ../binOut/net/ 2> /dev/null
javac @compiler.txt
