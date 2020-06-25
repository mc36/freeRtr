#!/bin/sh
CC="clang"                              #clang
CC="gcc"                                #gcc

compileFile()
{
echo compiling $1.
$CC -O3 -march=corei7 -o../../binTmp/$1.bin $2 $1.c $3
}

compileFile p4dpdk "-I /usr/include/dpdk/ -I /usr/include/x86_64-linux-gnu/dpdk" "-lpthread -lpcap -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"
for fn in p4pkt p4emu mapInt rawInt pcapInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread -lpcap"
  done
