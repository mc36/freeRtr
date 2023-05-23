proc doDisIP6pack {pack} {
global packetSimp
global packetVerb
set packetSimp "$packetSimp ip6="
set a [readByte $pack 0]
set a [expr $a >> 4]
set packetVerb "$packetVerb\IPv6 header\n  version=$a\n"
if {$a != 6} return
set a [readWord $pack 0]
set a [expr ($a >> 4) & 0xff]
set packetVerb "$packetVerb  traffic class=0x[toHex $a 2]\n"
set a [readLong $pack 0]
set a [expr $a & 0xfffff]
set packetVerb "$packetVerb  flow label=0x[toHex $a 5]\n"
set a [readWord $pack 4]
set packetVerb "$packetVerb  payload length=$a\n"
set a [expr (($a+40)*2)-1]
set pack [string range $pack 0 $a]
set prot [readByte $pack 6]
set a [readByte $pack 7]
set packetVerb "$packetVerb  next header=$prot\n  hop limit=$a\n"
set a [readAddr6 $pack 8]
set b [readAddr6 $pack 24]
set packetVerb "$packetVerb  source=$a\n  destination=$b\n"
set packetSimp "$packetSimp$a-->$b"
set a [string range $pack 80 6666]
doDisPROTOpack $prot $a
}
