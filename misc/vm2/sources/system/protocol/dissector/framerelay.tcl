proc doDisFRMRELpack {pack} {
global packetSimp
global packetVerb
set a [readByte $pack 0]
set b [readByte $pack 1]
set c [expr (($a >> 2) << 4) | ($b >> 4)]
set packetVerb "$packetVerb\Frame Relay header\n  dlci=$c\n  flags="
addFlagsValue $a 0x02 cr
addFlagsValue $a 0x01 ea
addFlagsValue $b 0x08 fecn
addFlagsValue $b 0x04 becn
addFlagsValue $b 0x02 de
addFlagsValue $b 0x01 ea
set packetVerb "$packetVerb\n"
doDisETHTYPpack [string range $pack 4 6666]
}
