proc doDisIP4pack {pack} {
global packetSimp
global packetVerb
set packetSimp "$packetSimp ip4="
set a [readByte $pack 0]
set b [expr $a >> 4]
set head [expr ($a & 15)*4]
set packetVerb "$packetVerb\IPv4 header\n  version=$b\n  header length=$head\n"
if {$b != 4} return
set a [readByte $pack 1]
set packetVerb "$packetVerb  type of service=0x[toHex $a 2]\n"
set a [readWord $pack 2]
set c [readWord $pack 4]
set packetVerb "$packetVerb  total length=$a\n  identification=0x[toHex $c 4]\n"
set b [expr ($a*2)-1]
set pack [string range $pack 0 $b]
set a [readWord $pack 6]
set packetVerb "$packetVerb  fragment offset=[expr $a & 0x1fff]\n  flags="
addFlagsValue $a 0x4000 dontfrag
addFlagsValue $a 0x2000 morefrag
set a [readByte $pack 8]
set prot [readByte $pack 9]
set c [readWord $pack 10]
set packetVerb "$packetVerb\n  time to live=$a\n  protocol=$prot\n  checksum=0x[toHex $c 4]\n"
set a [readAddr4 $pack 12]
set b [readAddr4 $pack 16]
set packetVerb "$packetVerb  source=$a\n  destination=$b\n"
set packetSimp "$packetSimp$a-->$b"
set a [expr $head*2]
set a [string range $pack $a 6666]
doDisPROTOpack $prot $a
}
