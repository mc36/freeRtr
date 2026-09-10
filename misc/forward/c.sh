#!/bin/sh

. ../native/i.sh

profProto()
{
echo -n "$1: "
rm $TR/$PR-$1.raw 2> /dev/null || true
$TR/p4profiler.bin p4bench_cmds.txt p4bench_$1.txt $TR/$PR-$1.raw || true
}

if [ "$PR" != "" ]; then
  compileFile p4profiler "" "-lcrypto" "-fprofile-generate"
  profProto ipv4
  profProto ipv6
  profProto vlan
  profProto pppoe
  profProto mpls
  rm $TR/p4profiler.bin || true
  llvm-profdata merge -output=$TR/$PR.res $TR/$PR-*.raw || true
  if [ -e $TR/$PR.res ]; then
    PR="-fprofile-use=$TR/$PR.res -Wno-backend-plugin"
  else
    PR=""
  fi
fi



for fn in p4xdp_pass p4xdp_drop p4xdp_kern p4xdp_krno p4mnl_kern; do
  compileBpf $fn
done

for fn in p4xdp_user; do
  compileFile $fn "" "-lpthread -lbpf" ""
done

for fn in p4mnl_user; do
  compileFile $fn "" "-lpthread -lbpf -lmnl" ""
done

for fn in p4full p4tiny p4huge p4dbg p4nocr p4none; do
  compileLib $fn "" "$PR"
done

for fn in p4pcap p4map p4raw p4xsk p4urng; do
  compileLib $fn "" ""
done

for fn in p4dpdk; do
  compileLib $fn "-I =/usr/include/dpdk/ -I =/usr/include/$UM-linux-$AB/dpdk" $MF
done

for fn in p4bench p4udp p4min; do
  compileWith $fn "p4full" "-lcrypto" ""
done


linkTwoLibs "p4emu" "p4pcap" "p4full" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4dbg" "p4pcap" "p4dbg" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4hug" "p4pcap" "p4huge" "-lpthread -lpcap -lcrypto"

linkTwoLibs "p4pkt" "p4pcap" "p4none" "-lpthread -lpcap"

linkTwoLibs "p4pln" "p4pcap" "p4nocr" "-lpthread -lpcap"

linkTwoLibs "p4tin" "p4pcap" "p4tiny" "-lpthread -lpcap"


linkTwoLibs "p4dpdk" "p4dpdk" "p4full" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkDbg" "p4dpdk" "p4dbg" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkHug" "p4dpdk" "p4huge" "-lpthread -lcrypto -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkPkt" "p4dpdk" "p4none" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkPln" "p4dpdk" "p4nocr" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"

linkTwoLibs "p4dpdkTin" "p4dpdk" "p4tiny" "-lpthread -lrte_eal -lrte_mempool -lrte_mbuf -lrte_ring -lrte_ethdev"


linkTwoLibs "p4map" "p4map" "p4full" "-lpthread -lcrypto"

linkTwoLibs "p4mapDbg" "p4map" "p4dbg" "-lpthread -lcrypto"

linkTwoLibs "p4mapHug" "p4map" "p4huge" "-lpthread -lcrypto"

linkTwoLibs "p4mapPkt" "p4map" "p4none" "-lpthread"

linkTwoLibs "p4mapPln" "p4map" "p4nocr" "-lpthread"

linkTwoLibs "p4mapTin" "p4map" "p4tiny" "-lpthread"


linkTwoLibs "p4raw" "p4raw" "p4full" "-lpthread -lcrypto"

linkTwoLibs "p4rawDbg" "p4raw" "p4dbg" "-lpthread -lcrypto"

linkTwoLibs "p4rawHug" "p4raw" "p4huge" "-lpthread -lcrypto"

linkTwoLibs "p4rawPkt" "p4raw" "p4none" "-lpthread"

linkTwoLibs "p4rawPln" "p4raw" "p4nocr" "-lpthread"

linkTwoLibs "p4rawTin" "p4raw" "p4tiny" "-lpthread"


linkTwoLibs "p4xsk" "p4xsk" "p4full" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskDbg" "p4xsk" "p4dbg" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskHug" "p4xsk" "p4huge" "-lpthread -lxdp -lcrypto"

linkTwoLibs "p4xskPkt" "p4xsk" "p4none" "-lpthread -lxdp"

linkTwoLibs "p4xskPln" "p4xsk" "p4nocr" "-lpthread -lxdp"

linkTwoLibs "p4xskTin" "p4xsk" "p4tiny" "-lpthread -lxdp"


linkTwoLibs "p4urng" "p4urng" "p4full" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngDbg" "p4urng" "p4dbg" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngHug" "p4urng" "p4huge" "-lpthread -luring -lcrypto"

linkTwoLibs "p4urngPkt" "p4urng" "p4none" "-lpthread -luring"

linkTwoLibs "p4urngPln" "p4urng" "p4nocr" "-lpthread -luring"

linkTwoLibs "p4urngTin" "p4urng" "p4tiny" "-lpthread -luring"
