proc nametonumber {n} {
set nm $n
set vl 0
set dot [string first "." $n]
if { $dot > 0 } {
set nm [string range $n 0 [expr $dot-1]]
set vl [string range $n [expr $dot+1] [string length $n]]
}
set tb 0
set pl 0
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
if {[string first $tn $nm] == 0} {
set tb $tv
set pl [string length $tn]
}
}
if {$tb < 1} {
return ""
}
set kk [string range $nm $pl [string length $nm]]
if {[string length $kk] < 1} {
return ""
}
if {[string first [string range $kk 0 0] "0123456789"] < 0} {
return ""
}
if { $dot > 0 } {
if {[string length $vl] < 1} {
return ""
}
if {[string first [string range $vl 0 0] "0123456789"] < 0} {
return ""
}
if {$kk > 199} {
return ""
}
if {$vl > 4999} {
return ""
}
set t [expr $kk*5000]
set t [expr $t+$vl]
set b [expr $tb*1000000]
return [expr $b+$t]
}
if {$kk > 9999} {
return ""
}
set t [expr $tb*10000]
return [expr $t+$kk]
}

proc fieldval {s k} {
set p [string first "$k is " $s]
if {$p < 0} {
return ""
}
set s [string range $s [expr $p+[string length $k]+4] [string length $s]]
set s [string trim $s]
set p [string first " " $s]
if {$p < 0} {
return $s
}
return [string range $s 0 [expr $p-1]]
}

proc rowval {t ifc n} {
set p [string first "\n$ifc " $t]
if {$p < 0} {
set p [string first "$ifc " $t]
if {$p < 0} {
return 0
}
if {$p > 0} {
return 0
}
} else {
set p [expr $p+1]
}
set t [string range $t $p [string length $t]]
set m [expr $n+1]
set i 1
set w ""
while { $i < $m }{
set t [string trim $t]
set q [string first " " $t]
if {$q < 0} {
set w $t
set t ""
} else {
set w [string range $t 0 [expr $q-1]]
set t [string range $t $q [string length $t]]
}
set i [expr $i+1]
}
set q [string first "\n" $w]
if {$q > 0} {
set w [string range $w 0 [expr $q-1]]
}
set q [string first "+" $w]
if {$q > 0} {
set a [string range $w 0 [expr $q-1]]
set c [string range $w [expr $q+1] [string length $w]]
set w [expr $a+$c]
}
return $w
}
