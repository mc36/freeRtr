proc doDisPPPpack {pack} {
global packetSimp
global packetVerb
set a [readByte $pack 0]
set b [readByte $pack 1]
set c [readWord $pack 2]
set packetVerb "$packetVerb\PPP header\n  address=0x[toHex $a 2]\n  control=0x[toHex $b 2]\n  type=0x[toHex $c 4]\n"
if {$c == 0x21} {
  doDisIP4pack [string range $pack 8 6666]
  return
  }
if {$c == 0x57} {
  doDisIP6pack [string range $pack 8 6666]
  return
  }
}
