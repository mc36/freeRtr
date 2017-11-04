proc nametonumber {n}{

set inttypenum [string first "ethernet" $n]

if { $inttypenum == 0} {

set k [string range $n 8 [string length $n]]

set number [expr 70000+$k]

return $number 

}


set inttypenum [string first "atm" $n]
if { $inttypenum == 0} {

set k [string range $n 3 [string length $n]]

set number [expr 20000+$k]

return $number

}



set inttypenum [string first "serial" $n]
if { $inttypenum == 0} {

set k [string range $n 6 [string length $n]]

set number [expr 90000+$k]

return $number

}


set inttypenum [string first "cellular" $n]
if { $inttypenum == 0} {

set k [string range $n 8 [string length $n]]

set number [expr 50000+$k]

return $number

}


set inttypenum [string first "loopback" $n]
if { $inttypenum == 0} {

set k [string range $n 8 [string length $n]]

set number [expr 80000+$k]

return $number

}

set inttypenum [string first "dialer" $n]
if { $inttypenum == 0} {

set k [string range $n 6 [string length $n]]

set number [expr 60000+$k]

return $number

}

set inttypenum [string first "bvi" $n]
if { $inttypenum == 0} {

set k [string range $n 3 [string length $n]]

set number [expr 40000+$k]

return $number

}

set inttypenum [string first "bundle" $n]
if { $inttypenum == 0} {

set k [string range $n 6 [string length $n]]

set number [expr 30000+$k]

return $number

}


set inttypenum [string first "tunnel" $n]
if { $inttypenum == 0} {

set k [string range $n 6 [string length $n]]

set number [expr 100000+$k]

return $number

}


set inttypenum [string first "access" $n]
if { $inttypenum == 0} {

set k [string range $n 6 [string length $n]]

set number [expr 10000+$k]

return $number

}

}


