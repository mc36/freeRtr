proc doDisETHTYPpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set b "ethertype=0x[toHex $a 4]"
if {$a == 0x8100} {
  doDisDOT1Qpack [string range $pack 4 6666]
  return
  }
if {$a == 0x8847} {
  doDisMPLSpack [string range $pack 4 6666]
  return
  }
if {$a == 0x800} {
  doDisIP4pack [string range $pack 4 6666]
  return
  }
if {$a == 0x86dd} {
  doDisIP6pack [string range $pack 4 6666]
  return
  }
if {$a == 0x8864} {
  doDisPPPOEpack [string range $pack 4 6666]
  return
  }
if {$a == 0x3cf} {
  doDisPPPpack ff03[string range $pack 4 6666]
  return
  }
if {$a < 0x800} {set b "llc"}
if {$a == 0x806} {set b "arp"}
set packetSimp "$packetSimp $b"
return
}
