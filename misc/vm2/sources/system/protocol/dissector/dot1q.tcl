proc doDisDOT1Qpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set b [expr $a & 0xfff]
set c [expr $a >> 13]
set packetVerb "$packetVerb\802.1q header\n  vlan=$b\n  cos=$c\n"
set packetSimp "$packetSimp vlan=$b"
doDisETHTYPpack [string range $pack 4 6666]
}
