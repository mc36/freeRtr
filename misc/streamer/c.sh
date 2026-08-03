#!/bin/sh
./d.sh

javac -source 11 -target 11 -Xlint:all -deprecation *.java

java packetSizes > /dev/null 2> io_cnst.h

for a in *.c ; do
  b="${a%.*}"
  echo compiling $b
  clang -O3 -Wall $b.c -o $b.bin -lasound -lm
  done
