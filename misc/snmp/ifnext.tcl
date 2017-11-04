proc if_next {z} {
if {[string equal $z ""]} {set z interface}
set l [exec "show interfaces summary"]
set l [split $l "\r"]
set l [split $l "\n"]
set p [string first $z $l]

if {$p == -1} {set p 9} else {set p [expr $p + [string length $z]]}



set l [string range $l $p 999999]
set l [string trim $l]
set p [string first " " $l]


if {$p == -1} {return ""}

set l [string range $l $p 999999]
set l [string trim $l]
set p [string first " " $l]

if {$p == -1} {return ""}


set l [string range $l $p 999999]
set l [string trim $l]
set p [string first " " $l]

if {$p == -1} {return ""}


set l [string range $l $p 999999]
set l [string trim $l]
set p [string first " " $l]

if {$p == -1} {return ""}


set l [string range $l $p 999999]
set l [string trim $l]
set p [string first " " $l]

if {$p == -1} {return ""}


return [string range $l 0 [expr $p-1]]
}


