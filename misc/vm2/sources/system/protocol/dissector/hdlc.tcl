proc doDisHDLCpack {pack} {
global packetSimp
global packetVerb
set a [readWord $pack 0]
if {$a != 0xf00} {return}
doDisETHTYPpack [string range $pack 4 6666]
}
