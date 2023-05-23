proc doDisETHERpack {pack} {
global packetSimp
global packetVerb
set a [readAddrMac $pack 0]
set b [readAddrMac $pack 6]
set c [readWord $pack 12]
set packetVerb "$packetVerb\Ethernet header\n  destination=$a\n  source=$b\n  type=0x[toHex $c 4]\n"
doDisETHTYPpack [string range $pack 24 6666]
}
