proc doDisICMP4pack {pack} {
global packetSimp
global packetVerb
set a [readByte $pack 0]
set b [readByte $pack 1]
set c [readWord $pack 2]
set d "unknown:$a"
if {$a == 8} {set d "echo-request"}
if {$a == 0} {set d "echo-reply"}
set packetSimp "$packetSimp icmp4=$d"
set packetVerb "$packetVerb\ICMPv4 header\n  type=$d\n  code=$b\n  checksum=0x[toHex $c 4]\n"
}
