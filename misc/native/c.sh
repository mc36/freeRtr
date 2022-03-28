#!/bin/sh
TR=../../binTmp
UM=`uname -m`
CC="tcc"                                #tcc
CC="llc"                                #llc
CC="gcc"                                #gcc
CC="clang"                              #clang

MD="-O0 -g"                             #devel
MD="-O3 -g"                             #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bt full
mkdir -p $TR

compileBpf()
{
echo compiling $1.
clang -Wall $MD -c -g -target bpf -I /usr/include/$UM-linux-gnu/ -o$TR/$1.bin $1.c
touch -d "2010-01-01 00:00:00" $TR/$1.bin
}

compileFile()
{
echo compiling $1.
$CC -Wall $MD $4 -o$TR/$1.bin $2 $1.c $3
strip $TR/$1.bin
touch -d "2010-01-01 00:00:00" $TR/$1.bin
}

for fn in p4xdp_pass p4xdp_kern; do
  compileBpf $fn
  done

for fn in p4xdp_user; do
  compileFile $fn "" "-lpthread -lbpf" ""
  done

for fn in p4bench; do
  compileFile $fn "" "-lcrypto" ""
  done

for fn in p4emu p4pkt; do
  compileFile $fn "" "-lpthread -lpcap -lcrypto" ""
  done

for fn in p4dpdk p4dpdkPkt; do
  compileFile $fn "-I /usr/include/dpdk/ -I /usr/include/$UM-linux-gnu/dpdk" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev" "-march=corei7"
  done

for fn in pcapInt pcap2pcap sender; do
  compileFile $fn "" "-lpthread -lpcap" ""
  done

for fn in mapInt rawInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread" ""
  done

for fn in ptyRun; do
  compileFile $fn "" "-lutil" ""
  done

for fn in dummyCon daemonRun; do
  compileFile $fn "" "" ""
  done
