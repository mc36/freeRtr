set z [string range $oid [expr [string last "." $oid]+1] [string length $oid]]
set interface ""
set suffix ""
set w [expr $z/10000]
set ifnum [expr $z%10000]
if {$z >= 1000000} {
set w [expr $z/1000000]
set r [expr $z%1000000]
set ifnum [expr $r/5000]
set v [expr $r%5000]
set suffix ".$v"
}
set p ""
set tab "access 1 atm 2 bundle 3 bvi 4 cellular 5 dialer 6 ethernet 7 loopback 8 serial 9 tunnel 10 hairpin 11 sdn 12"
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
if {$tv == $w} {
set interface "$tn$ifnum$suffix"
}
}
set b [exec "show interfaces $interface"]
