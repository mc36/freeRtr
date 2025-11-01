#!/bin/sh
TR=../../binTmp
mkdir -p $TR
UM=`uname -m`
MF=""
AB="gnu"
SR="/"

MD="-O0 -g --analyze"                   #pretesting
MD="-O0 -g -fsanitize=address"          #testing
MD="-O0 -g"                             #devel
MD="-O3 -g"                             #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bt full
#p *((struct <type> *)(<addr>))

if which clang > /dev/null ; then
  CC="clang"
  CS="llvm-strip"
  BC="clang -target bpf"
  BS="llvm-strip"
else
  CC="gcc"
  CS="strip"
  BC="bpf-gcc"
  BS="bpf-strip"
fi

while [ $# -gt 0 ]; do
  case $1 in
    md)
      MD=$2
      ;;
    um)
      UM=$2
      ;;
    ab)
      AB=$2
      ;;
    sr)
      SR=$2
      ;;
    cc)
      CC=$2
      ;;
    cs)
      CS=$2
      ;;
    bc)
      BC=$2
      ;;
    bs)
      BS=$2
      ;;
  esac
  shift 2
done

if [ "$UM" = "x86_64" ]; then
  MF="-march=corei7"
fi

echo arch=$UM, abi=$AB, sys=$SR, cc=$CC, cs=$CS, bc=$BC, bs=$BS, mode=$MD, flag=$MF, out=$TR

compileBpf()
{
echo compiling $1.
$BC --sysroot $SR -Wall $MD -c -g -I =/usr/include/ -I =/usr/include/$UM-linux-$AB/ -o$TR/$1.bin $1.c
$BS -d $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileLib()
{
echo compiling $1.
$CC --sysroot $SR -fpic -shared -Wall -Wl,--build-id=none $MD $3 -o$TR/lib$1.so $2 $1.c
chmod -x $TR/lib$1.so || true
$CS $TR/lib$1.so || true
touch -c -d "2010-01-01 00:00:00" $TR/lib$1.so || true
}

linkTwoLibs()
{
echo linking $1.
$CC --sysroot $SR -Wall -Wl,-rpath='$ORIGIN/' -Wl,--build-id=none $MD -o$TR/$1.bin -L$TR -l$2 -l$3 $4
$CS $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileFile()
{
echo compiling $1.
$CC --sysroot $SR -Wall -Wl,--build-id=none $MD $4 -o$TR/$1.bin $2 $1.c $3
$CS $TR/$1.bin || true
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

for fn in p4emu_full p4emu_tiny p4emu_huge p4emu_dbg p4emu_nocr p4emu_none p4emu_pcap p4emu_bench p4emu_udp p4emu_map p4emu_raw p4emu_xsk p4emu_urng; do
  compileLib $fn "" ""
done

for fn in p4emu_dpdk; do
  compileLib $fn "-I =/usr/include/dpdk/ -I =/usr/include/$UM-linux-$AB/dpdk" $MF
done

linkTwoLibs "p4emu" "p4emu_pcap" "p4emu_full" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4dbg" "p4emu_pcap" "p4emu_dbg" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4hug" "p4emu_pcap" "p4emu_huge" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4pkt" "p4emu_pcap" "p4emu_none" "-lpthread -lpcap"

linkTwoLibs "p4pln" "p4emu_pcap" "p4emu_nocr" "-lpthread -lpcap"

linkTwoLibs "p4tin" "p4emu_pcap" "p4emu_tiny" "-lpthread -lpcap"

linkTwoLibs "p4dpdk" "p4emu_dpdk" "p4emu_full" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkDbg" "p4emu_dpdk" "p4emu_dbg" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkHug" "p4emu_dpdk" "p4emu_huge" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkPkt" "p4emu_dpdk" "p4emu_none" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkPln" "p4emu_dpdk" "p4emu_nocr" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkTin" "p4emu_dpdk" "p4emu_tiny" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4bench" "p4emu_bench" "p4emu_full" "-lcrypto"

linkTwoLibs "p4udp" "p4emu_udp" "p4emu_full" "-lcrypto"

linkTwoLibs "p4map" "p4emu_map" "p4emu_full" "-lpthread -lcrypto"

linkTwoLibs "p4mapDbg" "p4emu_map" "p4emu_dbg" "-lpthread -lcrypto"

linkTwoLibs "p4mapHug" "p4emu_map" "p4emu_huge" "-lpthread -lcrypto"

linkTwoLibs "p4mapPkt" "p4emu_map" "p4emu_none" "-lpthread"

linkTwoLibs "p4mapPln" "p4emu_map" "p4emu_nocr" "-lpthread"

linkTwoLibs "p4mapTin" "p4emu_map" "p4emu_tiny" "-lpthread"

linkTwoLibs "p4raw" "p4emu_raw" "p4emu_full" "-lpthread -lcrypto"

linkTwoLibs "p4rawDbg" "p4emu_raw" "p4emu_dbg" "-lpthread -lcrypto"

linkTwoLibs "p4rawHug" "p4emu_raw" "p4emu_huge" "-lpthread -lcrypto"

linkTwoLibs "p4rawPkt" "p4emu_raw" "p4emu_none" "-lpthread"

linkTwoLibs "p4rawPln" "p4emu_raw" "p4emu_nocr" "-lpthread"

linkTwoLibs "p4rawTin" "p4emu_raw" "p4emu_tiny" "-lpthread"

linkTwoLibs "p4xsk" "p4emu_xsk" "p4emu_full" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskDbg" "p4emu_xsk" "p4emu_dbg" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskHug" "p4emu_xsk" "p4emu_huge" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskPkt" "p4emu_xsk" "p4emu_none" "-lpthread -lxdp"

linkTwoLibs "p4xskPln" "p4emu_xsk" "p4emu_nocr" "-lpthread -lxdp"

linkTwoLibs "p4xskTin" "p4emu_xsk" "p4emu_tiny" "-lpthread -lxdp"

linkTwoLibs "p4urng" "p4emu_urng" "p4emu_full" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngDbg" "p4emu_urng" "p4emu_dbg" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngHug" "p4emu_urng" "p4emu_huge" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngPkt" "p4emu_urng" "p4emu_none" "-lpthread -luring"

linkTwoLibs "p4urngPln" "p4emu_urng" "p4emu_nocr" "-lpthread -luring"

linkTwoLibs "p4urngTin" "p4emu_urng" "p4emu_tiny" "-lpthread -luring"

for fn in pcapInt pcap2pcap sender; do
  compileFile $fn "" "-lpthread -lpcap" ""
done

for fn in xskInt; do
  compileFile $fn "" "-lpthread -lxdp" ""
done

for fn in urngInt; do
  compileFile $fn "" "-lpthread -luring" ""
done

for fn in veth; do
  compileFile $fn "" "-lmnl" ""
done

for fn in mapInt cmp1int cmp2int rawInt tapInt bundle vlan fileInt hdlcInt syncInt syncClk asyncLin stdLin ttyLin modem; do
  compileFile $fn "" "-lpthread" ""
done

for fn in ptyRun; do
  compileFile $fn "" "-lutil" ""
done

for fn in seth rexec dummyCon daemonRun; do
  compileFile $fn "" "" ""
done
