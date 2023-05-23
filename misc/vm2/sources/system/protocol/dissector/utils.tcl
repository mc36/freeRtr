proc toHex {num digit} {
set a ""
while {$digit>0} {
  incr digit -1
  set i [expr $num & 15]
  set num [expr $num >> 4]
  set a [string range 0123456789abcdef $i $i]$a
  }
return $a
}


proc readByte {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
if {[string length $a] < 1} {set a "0"}
set a "0x[string range $a 0 1]"
set a [expr $a]
return $a
}


proc readWord {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
if {[string length $a] < 1} {set a "0"}
set a "0x[string range $a 0 3]"
set a [expr $a]
return $a
}


proc readLong {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
if {[string length $a] < 1} {set a "0"}
set a "0x[string range $a 0 7]"
set a [expr $a]
return $a
}



proc readAddr4 {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
return "[readByte $a 0].[readByte $a 1].[readByte $a 2].[readByte $a 3]"
}


proc readAddr6 {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
set b "[string range $a 0 3]:[string range $a 4 7]:"
set b "$b[string range $a 8 11]:[string range $a 12 15]:"
set b "$b[string range $a 16 19]:[string range $a 20 23]:"
set b "$b[string range $a 24 27]:[string range $a 28 31]"
return $b
}


proc readAddrMac {a o} {
set o [expr $o*2]
set a [string range $a $o 6666]
set b "[string range $a 0 3].[string range $a 4 7].[string range $a 8 11]"
return $b
}


proc addFlagsValue {flg val nam} {
global packetVerb
set flg [expr $flg & $val]
if {$flg == 0} {
  set nam [string tolower $nam]
  } else {
  set nam [string toupper $nam]
  }
set packetVerb "$packetVerb$nam "
}
