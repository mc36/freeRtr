proc if_next {z} {
set zz $z
if {[string equal $z ""]} {
set zz none
}
set txt [exec "show interfaces summary"]
set txt [split $txt "\r"]
set txt [split $txt "\n"]
set txt [string trim $txt]
set txt "$zz $txt"
set cur 0
set isfirst 1
set best ""
set bestn 0
set letters "abcdefghijklmnopqrstuvwxyz"
set digits "0123456789"
while { [string length $txt] > 0 }{
set pos [string first " " $txt]
if {$pos < 0} {
set tok $txt
set txt ""
} else {
set tok [string range $txt 0 [expr $pos-1]]
set txt [string range $txt $pos [string length $txt]]
set txt [string trim $txt]
}
set cand ""
set ch [string range $tok 0 0]
if {[string first $ch $letters] >= 0} {
set dot [string first "." $tok]
set nm $tok
set vl 0
if {$dot > 0} {
set vl [string range $tok [expr $dot+1] [string length $tok]]
set nm [string range $tok 0 [expr $dot-1]]
}
set tab "access 1 atm 2 bundle 3 bvi 4 cellular 5 dialer 6 ethernet 7 loopback 8 serial 9 tunnel 10 hairpin 11 sdn 12"
set tb 0
set pl 0
while { [string length $tab] > 0 }{
set q [string first " " $tab]
set tn [string range $tab 0 [expr $q-1]]
set tab [string range $tab $q [string length $tab]]
set tab [string trim $tab]
set q [string first " " $tab]
if {$q < 0} {
set tv $tab
set tab ""
} else {
set tv [string range $tab 0 [expr $q-1]]
set tab [string range $tab $q [string length $tab]]
set tab [string trim $tab]
}
if {[string first $tn $nm] == 0} {
set tb $tv
set pl [string length $tn]
}
}
if {$tb > 0} {
set kk [string range $nm $pl [string length $nm]]
if {[string length $kk] > 0} {
if {[string first [string range $kk 0 0] $digits] >= 0} {
if {$dot > 0} {
if {[string length $vl] > 0} {
if {[string first [string range $vl 0 0] $digits] >= 0} {
if {$kk < 200} {
if {$vl < 5000} {
set t [expr $kk*5000]
set t [expr $t+$vl]
set bb [expr $tb*1000000]
set cand [expr $bb+$t]
}
}
}
}
} else {
if {$kk < 10000} {
set t [expr $tb*10000]
set cand [expr $t+$kk]
}
}
}
}
}
}
if {$isfirst == 1} {
set isfirst 0
if {[string length $cand] > 0} {
set cur $cand
}
} else {
if {[string length $cand] > 0} {
if {$cand > $cur} {
if {[string length $best] < 1} {
set best $tok
set bestn $cand
}
if {$cand < $bestn} {
set best $tok
set bestn $cand
}
}
}
}
}
return $best
}
