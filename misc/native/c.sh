#!/bin/sh
LIBS="-lpthread -lwpcap"                #windows
LIBS="-lpthread -lpcap"                 #linux
CC="clang"                              #clang
CC="gcc"                                #gcc

for fn in mapInt rawInt pcapInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  echo compiling $fn.
  $CC -O3 -o../../binTmp/$fn.bin $fn.c $LIBS
  done
