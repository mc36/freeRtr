proc doDisL2TP2pack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set packetVerb "$packetVerb\l2tp v2 header\n  flags="
set packetSimp "$packetSimp l2tp2"
addFlagsValue $a 0x8000 type
addFlagsValue $a 0x4000 length
addFlagsValue $a 0x800 sequence
addFlagsValue $a 0x200 offset
addFlagsValue $a 0x100 priority
set b [expr $a & 15]
set packetVerb "$packetVerb\n  version=$b\n"
if {$b != 2} {return}
set b 2
if {($a & 0x4000) != 0} {
  set packetVerb "$packetVerb  length=[readWord $pack $b]\n"
  incr b 2
  }
set packetVerb "$packetVerb  tunnel id=[readWord $pack $b]\n"
incr b 2
set packetVerb "$packetVerb  session id=[readWord $pack $b]\n"
incr b 2
if {($a & 0x800) != 0} {
  set packetVerb "$packetVerb  ns=[readWord $pack $b]\n"
  incr b 2
  set packetVerb "$packetVerb  nr=[readWord $pack $b]\n"
  incr b 2
  }
if {($a & 0x200) != 0} {
  set c [readWord $pack $b]
  set packetVerb "$packetVerb  offset=$c\n"
  incr b 2
  incr b $c
  }
if {($a & 0x8000) != 0} {return}
set b [expr $b * 2]
doDisPPPpack [string range $pack $b 6666]
}
