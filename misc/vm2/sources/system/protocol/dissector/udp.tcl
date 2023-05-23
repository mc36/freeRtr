proc doDisUDPpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set b [readWord $pack 2]
set c [readWord $pack 4]
set d [readWord $pack 6]
set e [expr $c - 8]
set packetSimp "$packetSimp udp=$a-->$b data=$e"
set packetVerb "$packetVerb\UDP header\n  source=$a\n  destination=$b\n"
set packetVerb "$packetVerb  length=$c\n  checksum=0x[toHex $d 4]\n"
if {($a == 1701) & ($b == 1701)} {
  doDisL2TP2pack [string range $pack 16 6666]
  return
  }
return
}
