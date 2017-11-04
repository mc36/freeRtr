#!/bin/sh
LIBS="-lpthread -lwpcap"                #windows
LIBS="-lpthread -lpcap"                 #linux

for fn in mapInt rawInt pcapInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  echo compiling $fn.
  gcc -O3 -o../../binTmp/$fn.bin $fn.c $LIBS
  done
