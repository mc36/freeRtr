proc doDisGREpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
set b [readWord $pack 2]
set packetVerb "$packetVerb\GRE header\n  version=[expr $a & 7]\n  type=0x[toHex $b 4]\n  flags="
set packetSimp "$packetSimp gre"
set pack [string range $pack 8 6666]
addFlagsValue 0x8000 $a chkSum
addFlagsValue 0x2000 $a key
addFlagsValue 0x1000 $a seq
set packetVerb "$packetVerb\n"
if {($a & 0x8000) != 0} {
  set pack [string range $pack 8 6666]
  }
if {($a & 0x2000) != 0} {
  set c [readLong $pack 0]
  set pack [string range $pack 8 6666]
  set packetVerb "$packetVerb\  key=$c\n"
  }
if {($a & 0x1000) != 0} {
  set c [readLong $pack 0]
  set pack [string range $pack 8 6666]
  set packetVerb "$packetVerb\  sequence=$c\n"
  }
doDisETHTYPpack "[toHex $b 4]$pack"
}
