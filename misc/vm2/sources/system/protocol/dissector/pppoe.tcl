proc doDisPPPOEpack {pack} {
global packetSimp
global packetVerb
set packetVerb "$packetVerb\PPPoE header\n"
set packetSimp "$packetSimp pppoe"
set a [readByte $pack 0]
set b [readByte $pack 1]
set c [readWord $pack 2]
set d [readWord $pack 4]
set packetVerb "$packetVerb  version=[expr $a >> 4]\n  type=[expr $a & 15]\n  session id=$c\n  length=$d\n"
doDisPPPpack ff03[string range $pack 12 6666]
}
