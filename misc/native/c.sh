#!/bin/sh
CC="tcc"                                #tcc
CC="llc"                                #llc
CC="gcc"                                #gcc
CC="clang"                              #clang

MD="-O0 -g"                             #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bt full

compileFile()
{
echo compiling $1.
$CC -Wall $MD $4 -o../../binTmp/$1.bin $2 $1.c $3
touch -d "2010-01-01 00:00:00" ../../binTmp/$1.bin
}

if [ "`dpkg -s dpdk-dev | grep Version`" \< "Version: 20" ] ; then
  patch -p0 < dpdk19.patch
  fi

for fn in p4xdp_pass p4xdp_kern; do
  compileFile $fn "-c -g" "-target bpf" "-I /usr/include/`uname -m`-linux-gnu/"
  done

for fn in p4xdp_user; do
  compileFile $fn "" "-lpthread -lbpf" ""
  done

for fn in p4emu p4pkt; do
  compileFile $fn "" "-lpthread -lpcap -lcrypto" ""
  done

for fn in p4dpdk p4dpdkPkt; do
  compileFile $fn "-I /usr/include/dpdk/ -I /usr/include/`uname -m`-linux-gnu/dpdk" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev" "-march=corei7"
  done

for fn in pcapInt pcap2pcap sender; do
  compileFile $fn "" "-lpthread -lpcap" ""
  done

for fn in mapInt rawInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread" ""
  done
