#!/bin/sh
./d.sh
javac -source 11 -target 11 -Xlint:all -deprecation *.java
for a in *.c ; do
  b="${a%.*}"
  echo compiling $b
  clang -O3 $b.c -o $b.bin -lasound
  done
