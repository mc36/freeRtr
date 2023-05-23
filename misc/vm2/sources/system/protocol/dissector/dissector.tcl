source utils.tcl

source ether.tcl
source framerelay.tcl
source hdlc.tcl
source ppp.tcl
source ethtyp.tcl
source dot1q.tcl
source pppoe.tcl
source mpls.tcl

source ip4.tcl
source ip6.tcl
source proto.tcl
source icmp4.tcl
source icmp6.tcl
source gre.tcl
source l2tpv3.tcl

source tcp.tcl
source udp.tcl

source l2tpv2.tcl




proc doDisOnePack {type pack} {
set pack [string map {" " ""} $pack]
set pack [string tolower $pack]
global packetSimp
global packetVerb
set packetSimp ""
set packetVerb ""
doDis$type\pack $pack
}



proc doDisplayCurrent {} {
global packetSimp
global packetVerb
set a "-------------------<separator>-------------------"
puts "$a beg\n$packetSimp\n$a mid\n$packetVerb$a end"
}
