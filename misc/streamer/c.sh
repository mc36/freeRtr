#!/bin/sh
./d.sh

echo '#include "io_cnst.h"
public class consts {
public static final int rate = srate;
public static final int smpb = smpbt;
public static final int payl = pktln;
public static final int rtpl = padln; }
' | clang -E - | grep public > consts.java


javac -source 11 -target 11 -Xlint:all -deprecation *.java
for a in *.c ; do
  b="${a%.*}"
  echo compiling $b
  clang -O3 -Wall $b.c -o $b.bin -lasound -lm
  done
