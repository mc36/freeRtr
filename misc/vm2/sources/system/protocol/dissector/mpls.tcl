proc doDisMPLSpack {pack} {
global packetSimp
global packetVerb
set a [readLong $pack 0]
set c [expr $a & 0xff]
set a [expr $a >> 8]
set b [expr $a >> 4]
set d [expr ($a >> 1) & 7]
set e [expr $a & 1]
set packetVerb "$packetVerb\MPLS header\n  label=$b\n  experimental=$d\n  time to live=$c\n  bottom of stack=$e\n"
set packetSimp "$packetSimp mpls=$b"
set a [string range $pack 8 6666]
if {$e == 0} {doDisMPLSpack $a} else {doDisIP4pack $a}
}
