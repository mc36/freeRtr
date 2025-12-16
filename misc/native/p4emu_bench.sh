#!/bin/sh

benchProto()
{
echo --------------------------- benchmarking $1 packets
taskset 1 ../../binTmp/p4bench.bin p4emu_bench_cmds.txt 50000000 p4emu_bench_$1.txt | grep -v buffer
}

benchProto ipv4
benchProto ipv6
benchProto vlan
benchProto pppoe
benchProto mpls
