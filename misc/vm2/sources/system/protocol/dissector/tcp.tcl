proc doDisTCPpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set b [readWord $pack 2]
set packetSimp "$packetSimp tcp=$a-->$b"
set packetVerb "$packetVerb\TCP header\n  source=$a\n  destination=$b\n"
set a [readLong $pack 4]
set b [readLong $pack 8]
set offs [readByte $pack 12]
set offs [expr ($offs >> 4)*4]
set data [string length $pack]
set data [expr ($data/2) - $offs]
set packetVerb "$packetVerb  sequence=0x[toHex $a 8]  (next=0x[toHex [expr $data+$a] 8])\n"
set packetVerb "$packetVerb  acknowledge=0x[toHex $b 8]\n"
set packetVerb "$packetVerb  data offset=$offs  (data size=$data)\n  flags="
set flg [readWord $pack 12]
addFlagsValue $flg 0x01 fin
addFlagsValue $flg 0x02 syn
addFlagsValue $flg 0x04 rst
addFlagsValue $flg 0x08 psh
addFlagsValue $flg 0x10 ack
addFlagsValue $flg 0x20 urg
addFlagsValue $flg 0x40 ece
addFlagsValue $flg 0x80 cwr
set a [readWord $pack 14]
set b [readWord $pack 16]
set c [readWord $pack 18]
set packetVerb "$packetVerb\n  window size=$a\n"
set packetVerb "$packetVerb  checksum=0x[toHex $b 4]\n"
set packetVerb "$packetVerb  urgent pointer=$c\n"
set packetSimp "$packetSimp data=$data"
set a ""
if {[expr $flg & 1] !=0} { set a "$a fin" }
if {[expr $flg & 2] !=0} { set a "$a syn" }
if {[expr $flg & 4] !=0} { set a "$a rst" }
set packetSimp "$packetSimp$a"
}
