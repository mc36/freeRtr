proc doDisL2TP3pack {pack} {
global packetSimp
global packetVerb
set a [readLong $pack 0]
set packetVerb "$packetVerb\l2tp v3 header\n  sessionid=0x[toHex $a 8]\n"
set packetSimp "$packetSimp l2tp3"
if {$a == 0} {return}
doDisPPPpack ff03[string range $pack 8 6666]
}
