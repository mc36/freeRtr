proc doDisICMP6pack {pack} {
global packetSimp
global packetVerb
set a [readByte $pack 0]
set b [readByte $pack 1]
set c [readWord $pack 2]
set d "unknown:$a"
if {$a == 128} {set d "echo-request"}
if {$a == 129} {set d "echo-reply"}
if {$a == 135} {set d "neighbor solicit"}
if {$a == 136} {set d "neighbor advertise"}
set packetSimp "$packetSimp icmp6=$d"
set packetVerb "$packetVerb\ICMPv6 header\n  type=$d\n  code=$b\n  checksum=0x[toHex $c 4]\n"
}
