#!/bin/sh
LIBS="-lpthread -lpcap -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"
INCS="-I /usr/include/dpdk/ -I /usr/include/x86_64-linux-gnu/dpdk"
CC="clang"                              #clang
CC="gcc"                                #gcc

for fn in p4pkt p4emu p4dpdk mapInt rawInt pcapInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  echo compiling $fn.
  $CC -O3 -march=corei7 -o../../binTmp/$fn.bin $INCS $fn.c $LIBS
  done
