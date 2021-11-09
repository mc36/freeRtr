#!/bin/sh
CC="tcc"                                #tcc
CC="clang"                              #clang
CC="gcc"                                #gcc

MD="-g"                                 #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bf -full

compileFile()
{
echo compiling $1.
$CC -Wall $MD $4 -o../../binTmp/$1.bin $2 $1.c $3
touch -d "2010-01-01 00:00:00" ../../binTmp/$1.bin
}

for fn in p4dpdk p4dpdkPkt; do
  compileFile $fn "-I /usr/include/dpdk/ -I /usr/include/x86_64-linux-gnu/dpdk" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev" "-march=corei7"
  done

for fn in p4emu p4pkt; do
  compileFile $fn "" "-lpthread -lpcap -lcrypto" ""
  done

for fn in pcapInt pcap2pcap sender; do
  compileFile $fn "" "-lpthread -lpcap" ""
  done

for fn in mapInt rawInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread" ""
  done

