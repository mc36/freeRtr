#!/bin/sh
TR=../../binTmp
UM=`uname -m`

if which clang > /dev/null ; then
  CC="clang"
  BC="clang -target bpf"
  BS="llvm-strip"
  else
  CC="gcc"
  BC="bpf-gcc"
  BS="bpf-strip"
  fi

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

echo arch=$UM, cc=$CC, bc=$BC, bs=$BS, mode=$MD, flag=$MF, out=$TR

compileBpf()
{
echo compiling $1.
$BC -Wall $MD -c -g -I /usr/include/ -I /usr/include/$UM-linux-gnu/ -o$TR/$1.bin $1.c
$BS -d $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileLib()
{
echo compiling $1.
$CC -fpic -shared -Wall -Wl,--build-id=none $MD $3 -o$TR/lib$1.so $2 $1.c
chmod -x $TR/lib$1.so || true
strip $TR/lib$1.so || true
touch -c -d "2010-01-01 00:00:00" $TR/lib$1.so || true
}

linkTwoLibs()
{
echo linking $1.
$CC -Wall -Wl,-rpath='$ORIGIN/' -Wl,--build-id=none $MD -o$TR/$1.bin -L$TR -l$2 -l$3 $4
strip $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileFile()
{
echo compiling $1.
$CC -Wall -Wl,--build-id=none $MD $4 -o$TR/$1.bin $2 $1.c $3
strip $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}



for fn in p4xdp_pass p4xdp_drop p4xdp_kern p4xdp_krno p4mnl_kern; do
  compileBpf $fn
  done

for fn in p4xdp_user; do
  compileFile $fn "" "-lpthread -lbpf" ""
  done

for fn in p4mnl_user; do
  compileFile $fn "" "-lpthread -lbpf -lmnl" ""
  done

for fn in p4emu_full p4emu_dbg p4emu_nocr p4emu_none p4emu_pcap p4emu_bench p4emu_udp p4emu_map p4emu_raw p4emu_xsk p4emu_urng; do
  compileLib $fn "" ""
  done

for fn in p4emu_dpdk; do
  compileLib $fn "-I /usr/include/dpdk/ -I /usr/include/$UM-linux-gnu/dpdk" $MF
  done

linkTwoLibs "p4emu" "p4emu_pcap" "p4emu_full" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4dbg" "p4emu_pcap" "p4emu_dbg" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4pkt" "p4emu_pcap" "p4emu_none" "-lpthread -lpcap"

linkTwoLibs "p4dpdk" "p4emu_dpdk" "p4emu_full" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkDbg" "p4emu_dpdk" "p4emu_dbg" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkPkt" "p4emu_dpdk" "p4emu_none" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4bench" "p4emu_bench" "p4emu_full" "-lcrypto"

linkTwoLibs "p4udp" "p4emu_udp" "p4emu_full" "-lpthread -lcrypto"

linkTwoLibs "p4map" "p4emu_map" "p4emu_full" "-lpthread -lcrypto"

linkTwoLibs "p4mapDbg" "p4emu_map" "p4emu_dbg" "-lpthread -lcrypto"

linkTwoLibs "p4mapPkt" "p4emu_map" "p4emu_none" "-lpthread"

linkTwoLibs "p4raw" "p4emu_raw" "p4emu_full" "-lpthread -lcrypto"

linkTwoLibs "p4rawDbg" "p4emu_raw" "p4emu_dbg" "-lpthread -lcrypto"

linkTwoLibs "p4rawPkt" "p4emu_raw" "p4emu_none" "-lpthread"

linkTwoLibs "p4xsk" "p4emu_xsk" "p4emu_full" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskDbg" "p4emu_xsk" "p4emu_dbg" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskPkt" "p4emu_xsk" "p4emu_none" "-lpthread -lxdp"

linkTwoLibs "p4urng" "p4emu_urng" "p4emu_full" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngDbg" "p4emu_urng" "p4emu_dbg" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngPkt" "p4emu_urng" "p4emu_none" "-lpthread -luring"

for fn in pcapInt pcap2pcap sender; do
  compileFile $fn "" "-lpthread -lpcap" ""
  done

for fn in xskInt; do
  compileFile $fn "" "-lpthread -lxdp" ""
  done

for fn in urngInt; do
  compileFile $fn "" "-lpthread -luring" ""
  done

for fn in mapInt rawInt tapInt bundle vlan hdlcInt stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread" ""
  done

for fn in ptyRun; do
  compileFile $fn "" "-lutil" ""
  done

for fn in dummyCon daemonRun vm; do
  compileFile $fn "" "" ""
  done
