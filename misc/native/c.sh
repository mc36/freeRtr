#!/bin/sh
TR=../../binTmp
UM=`uname -m`
CC="tcc"                                #tcc
CC="gcc"                                #gcc
CC="clang"                              #clang

MD="-O0 -g"                             #devel
MD="-O3 -g"                             #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bt full
#p *((struct <type> *)(<addr>))
mkdir -p $TR

MF=""
if [ "$UM" = "x86_64" ]; then
  MF="-march=corei7"
fi
if [ "$UM" = "i686" ]; then
  MF="-march=corei7"
fi

echo arch=$UM, cc=$CC, mode=$MD, flag=$MF, out=$TR

compileBpf()
{
echo compiling $1.
clang -Wall $MD -c -g -target bpf -I /usr/include/$UM-linux-gnu/ -o$TR/$1.bin $1.c
llvm-strip -d $TR/$1.bin || true
touch -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileLib()
{
echo precompiling $1.
$CC -fpic -shared -Wall $MD $3 -o$TR/lib$1.so $2 $1.c
chmod -x $TR/lib$1.so
touch -d "2010-01-01 00:00:00" $TR/lib$1.so || true
}

linkTwoLibs()
{
echo linking $1.
$CC -Wall -Wl,-rpath='$ORIGIN/' $MD -o$TR/$1.bin -L$TR -l$2 -l$3 $4
strip $TR/$1.bin || true
}

compileFile()
{
echo compiling $1.
$CC -Wall $MD $4 -o$TR/$1.bin $2 $1.c $3
strip $TR/$1.bin || true
touch -d "2010-01-01 00:00:00" $TR/$1.bin || true
}



compileFile "vm2" "" "" ""

for fn in p4xdp_pass p4xdp_drop p4xdp_kern p4mnl_kern; do
  compileBpf $fn
  done

for fn in p4xdp_user; do
  compileFile $fn "" "-lpthread -lbpf" ""
  done

for fn in p4mnl_user; do
  compileFile $fn "" "-lpthread -lbpf -lmnl" ""
  done

for fn in p4emu_full p4emu_dbg p4emu_none p4emu_pcap p4emu_bench p4emu_udp; do
  compileLib $fn "" ""
  done

for fn in p4emu_dpdk; do
  compileLib $fn "-I /usr/include/dpdk/ -I /usr/include/$UM-linux-gnu/dpdk" $MF
  done

dpkdLibs="-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4emu" "p4emu_pcap" "p4emu_full" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4dbg" "p4emu_pcap" "p4emu_dbg" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4pkt" "p4emu_pcap" "p4emu_none" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4dpdk" "p4emu_dpdk" "p4emu_full" "$dpkdLibs"

linkTwoLibs "p4dpdkDbg" "p4emu_dpdk" "p4emu_dbg" "$dpkdLibs"

linkTwoLibs "p4dpdkPkt" "p4emu_dpdk" "p4emu_none" "$dpkdLibs"

linkTwoLibs "p4bench" "p4emu_bench" "p4emu_full" "-lcrypto"

linkTwoLibs "p4udp" "p4emu_udp" "p4emu_full" "-lpthread -lcrypto"

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
