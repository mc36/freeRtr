oid .* 0.1
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.




oid .* 1
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.



oid .* 1.3
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.


oid .* 1.3.6
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"
}

.


oid .* 1.3.6.1
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.



oid .* 1.3.6.1
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.



oid .* 1.3.6.1.2
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.


oid .* 1.3.6.1.2.1
proc snmp {mode oid} {

puts "1.3.6.1.2.1.1.1.0"

}

.



oid .* 1.3.6.1.2.1.2
proc snmp {mode oid} {

script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.1.$v"

}

.




oid .* 1.3.6.1.2.1.2.2
proc snmp {mode oid} {

script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.1.$v"

}

.






oid .* 1.3.6.1.2.1.2.2.1
proc snmp {mode oid} {

if {[string equal $mode next]} { } else {return}


script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.1.$v"

}

.

oid .* 1.3.6.1.2.1.2.2.1.1
proc snmp {mode oid} {

if {[string equal $mode next]} { } else {return}


script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.1.$v"

}
.

oid .* 1.3.6.1.2.1.2.2.1.1\..*
proc snmp {mode oid} {

script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.1.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.2.$thisn"
       }
	return
}

set m [string last "." $oid]
incr m
set index [string range $oid $m [string length $oid]]



puts "tag=integer int32=$index"

}

.

oid .* 1.3.6.1.2.1.2.2.1.1
proc snmp {mode oid} {

script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.1.$v"

}

.

oid .* 1.3.6.1.2.1.2.2.1.2\..*
proc snmp {mode oid} {
	
script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.2.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.3.$thisn"
       }
	return
}


puts "tag=octetstring string=$interface"


}

.









oid .* 1.3.6.1.2.1.2.2.1.2
proc snmp {mode oid} {

script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

set q [if_next]
set v [nametonumber $q]

puts "1.3.6.1.2.1.2.2.1.2.$v"

}

.

oid .* 1.3.6.1.2.1.2.2.1.5.*
proc snmp {mode oid} {


script getword.tcl



script intnum.tcl

script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.5.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.6.$thisn"
       }
	return
}




set start [string first "bw" $b]

set cut [string range $b $start [string length $b]]

set startt [string first , $cut]
set startt [expr $startt -1 ]

set final [string range $cut 0 $startt]


set result [string range $final [expr [string first "=" $final]+1]  [string length $final]]


set a [string first "m" $result]
set r [expr $a-1]
set bw [string range $result 0 $r]


puts "tag=integer class=1 int32=$bw"




}
.


oid .* 1.3.6.1.2.1.2.2.1.6.*
proc snmp {mode oid} {


script getword.tcl


script intnum.tcl

script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.6.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.7.$thisn"
       }
	return
}



set start [string first "hw" $b]

set cut [string range $b $start [string length $b]]

set startt [string first , $cut]
set startt [expr $startt -1 ]

set final [string range $cut 0 $startt]


set result [string range $final [expr [string first "=" $final]+1]  [string length $final]]


puts "tag=octetstring string= $result"




}
.



oid .* 1.3.6.1.2.1.2.2.1.4.*



proc snmp {mode oid} {


script getword.tcl

script intnum.tcl

script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.4.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.5.$thisn"
       }
	return
}

set start [string first "mtu" $b]

set cut [string range $b $start [string length $b]]

set startt [string first , $cut]
set startt [expr $startt -1 ]

set final [string range $cut 0 $startt]


set result [string range $final [expr [string first "=" $final]+1]  [string length $final]]


puts "tag=integer int32=$result"




}
.

oid .* 1.3.6.1.2.1.2.2.1.7.*
proc snmp {mode oid} {


script getword.tcl


script intnum.tcl

script ifname.tcl

script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.7.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.8.$thisn"
       }
	
	return
}




getword
getword
set final [getword]

set result 0
set comp 0
set comp2 0

set comp [string first "u" $final]

if { $comp > -1 }{
	set result 1
}

set comp [string first "d" $final]

if { $comp > -1 }{
	set result 2
}



puts "tag=integer int32=$result"




}
.



oid .* 1.3.6.1.2.1.2.2.1.8.*
proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.8.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.10.$thisn"
       }
	
	return
}




getword
getword
set final [getword]
set result 0
set comp 0
set comp2 0
set comp [string first "u" $final]

if { $comp > -1 }{
	set result 1
}

set comp [string first "d" $final]

if { $comp > -1 }{
	set result 2
}



puts "tag=integer int32=$result"
}
.

oid .* 1.3.6.1.2.1.2.2.1.10.*
proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.10.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.17.$thisn"
       }
	
	return
}
set total [ exec "show interfaces total" ]
set b [ string range $total [string first $interface $total] [string length $total]]
getword
getword
set result [getword]
puts "tag=1 class=1 int32=$result"
}
.



oid .* 1.3.6.1.2.1.2.2.1.11.*
proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.11.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.14.$thisn"
       }
	
	return
}
set total [ exec "show interfaces ptotal" ]
set b [ string range $total [string first $interface $total] [string length $total]]
getword
getword
set result [getword]
puts "tag=1 class=1 int32=$result"
}
.


oid .* 1.3.6.1.2.1.2.2.1.14.*
proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.14.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.16.$thisn"
       }
	
	return
}

set total [ exec "show interfaces ptotal" ]
set b [ string range $total [string first $interface $total] [string length $total]]
set b [split $b "\r"]
set b [split $b "\n"]
getword
getword
getword
getword
set result [getword]
puts "tag=1 class=1 int32=$result"



}
.

oid .* 1.3.6.1.2.1.2.2.1.16.*
proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.16.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.17.$thisn"
       }
	
	return
}

set total [ exec "show interfaces total" ]
set b [ string range $total [string first $interface $total] [string length $total]]
getword
getword
set result [getword]
puts "tag=1 class=1 int32=$result"

}
.


oid .* 1.3.6.1.2.1.2.2.1.17.*
proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.17.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.20.$thisn"
       }
	
	return
}
set total [ exec "show interfaces ptotal" ]
set b [ string range $total [string first $interface $total] [string length $total]]
getword
set result [getword]
puts "tag=1 class=1 int32=$result"
}
.

oid .* 1.3.6.1.2.1.2.2.1.20.*
proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.20.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.4.20.1.1"
       }
	
	return
}
set total [ exec "show interfaces ptotal" ]
set b [ string range $total [string first $interface $total] [string length $total]]
set b [split $b "\r"]
set b [split $b "\n"]
getword
getword
getword
getword
set result [getword]
puts "tag=1 class=1 int32=$result"
}
.




oid .* 1.3.6.1.2.1.2.2.1.3.*
proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.3.$thisn"
       } 
	else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.2.2.1.4.$thisn"
       }
	return
}


set comp 0
set ma "type"

set word ""
while { $comp < 1 }{
	set word [getword]
	set comp [string match $ma $word]
}
getword
set typ [getword]
set out 0
set eth "ethernet,"
set comp [string match $typ $eth]
if { $comp > 0 }{
	set out 6
}

set atm "atm,"
set comp [string match $typ $atm]
if { $comp > 0 }{
	set out 37
}

set ser "serial,"
set comp [string match $typ $ser]
if { $comp > 0 }{
	set out 22
}


set tun "gre,"
set comp [string match $typ $tun]
if { $comp > 0 }{
	set out 131
}



puts "tag=integer int32=$out"




}
.



oid .* 1.3.6.1.2.1.1
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.1.0"
}

.

oid .* 1.3.6.1.2.1.1.1
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.1.0"
}

.


oid .* 1.3.6.1.2.1.1.1.0
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.1.2.0"
		return
	}
	puts "tag=octetstring string=T-Systems Mobile Access Router"
	
}

.

oid .* 1.3.6.1.2.1.1.2
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.2.0"
}

.



oid .* 1.3.6.1.2.1.1.2.0
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.1.3.0"
		return
	}
	puts "tag=objectid oid=1.2.3.4"
	
}

.

oid .* 1.3.6.1.2.1.1.3
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.3.0"
}

.


oid .* 1.3.6.1.2.1.1.3.0
proc snmp {mode oid} {

script getword.tcl

if {[string equal $mode next]} {
	
	puts "1.3.6.1.2.1.1.5.0"
	return
}

set b [exec "show platform"]

set comp 0
set ma "for"

set word ""
while { $comp < 1 }{
	set word [getword]
	set comp [string match $ma $word]
	
}

set time [getword]
set time [split $time "\r"]
set time [split $time "\n"]


set fk [string first ":" $time]
set fk [expr $fk-1]
set hour [string range $time 0 $fk]


set fk [expr $fk+2]
set time [string range $time $fk [string length $time]]
set fk [string first ":" $time]
set fk [expr $fk-1]
set min [string range $time 0 $fk]

set fk [expr $fk+2]
set time [string range $time $fk [string length $time]]


set sec [string range $time 0 1]


set mm [expr 60*$min]
set sysup [expr $sec + $mm]
set hm [expr 3600*$hour]
set sysup [expr $sysup + $hm]
set sysup [expr $sysup*100]


puts "tag=3 class=1 int32=$sysup"

}

.

oid .* 1.3.6.1.2.1.1.5
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.5.0"
}

.


oid .* 1.3.6.1.2.1.1.5.0
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.1.6.0"
		return
	}



script getword.tcl

set b [exec "show platform"]

set b [split $b "\r"]

set b [split $b "\n"]


set m [getword]


set comp [string first "name:" $m]
while { $comp < 0 }{

	set m [getword]
		
 	set comp [string first "name:" $m]
}

set m [getword]


puts "tag=octetstring string=$m"

	
}

.

oid .* 1.3.6.1.2.1.1.6
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.1.6.0"
}

.


oid .* 1.3.6.1.2.1.1.6.0
proc snmp {mode oid} {
script getword.tcl


script intnum.tcl


script ifname.tcl

script ifnext.tcl

	if {[string equal $mode next]} {
	


	set q [if_next]
	set v [nametonumber $q]

	puts "1.3.6.1.2.1.2.2.1.1.$v"
		return
	}
	puts "tag=octetstring string=kfkibavan"
	
}

.

oid .* 1.3.6.1.2.1.4.20.1.1.*

proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.4.20.1.2"
		return
	}

puts "tag=0 class=1  adrip4=10.10.10.10"
}
.


oid .* 1.3.6.1.2.1.4.20.1.2.*

proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.4.20.1.3"
		return
	}

puts "tag=integer int32=0"
}
.


oid .* 1.3.6.1.2.1.4.20.1.3.*

proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.4.22.1"
		return
	}

puts "tag=0 class=1 string=10.10.10.10"
}
.



oid .* 1.3.6.1.2.1.4.20.1

proc snmp {mode oid} {


	puts "1.3.6.1.2.1.4.20.1.1"
}
.

oid .* 1.3.6.1.2.1.4.20

proc snmp {mode oid} {



	puts "1.3.6.1.2.1.4.20.1.1"
}
.

oid .* 1.3.6.1.2.1.4

proc snmp {mode oid} {


	puts "1.3.6.1.2.1.4.20.1.1"
}
.






oid .* 1.3.6.1.2.1.47.1.1.1.1.2
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.3"
		return
	}

puts "tag=octetstring string= asd"

}
.

oid .* 1.3.6.1.2.1.47.1.1.1.1

proc snmp {mode oid} {


	puts "1.3.6.1.2.1.47.1.1.1.1.2"
}
.

oid .* 1.3.6.1.2.1.47.1.1.1

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.47.1.1.1.1.2"
}
.

oid .* 1.3.6.1.2.1.47.1.1

proc snmp {mode oid} {
	puts "1.3.6.1.2.1.47.1.1.1.1.2"
}
.

oid .* 1.3.6.1.2.1.47.1

proc snmp {mode oid} {
	puts "1.3.6.1.2.1.47.1.1.1.1.2"
}
.

oid .* 1.3.6.1.2.1.47

proc snmp {mode oid} {
	puts "1.3.6.1.2.1.47.1.1.1.1.2"
}
.


oid .* 1.3.6.1.2.1.47.1.1.1.1.3
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.5"
		return
	}

puts "tag=octetstring string= asd"

}
.

oid .* 1.3.6.1.2.1.47.1.1.1.1.5
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.7"
		return
	}

puts "tag=octetstring string= asd"

}
.

oid .* 1.3.6.1.2.1.47.1.1.1.1.7
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.11"
		return
	}

puts "tag=octetstring string= asd"

}
.


oid .* 1.3.6.1.2.1.47.1.1.1.1.11
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.13"
		return
	}

puts "tag=octetstring string= asd"

}
.


oid .* 1.3.6.1.2.1.47.1.1.1.1.13
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.47.1.1.1.1.16"
		return
	}

puts "tag=octetstring string= asd"

}
.


oid .* 1.3.6.1.2.1.47.1.1.1.1.16
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.68.1.4.1.2"
		return
	}

puts "tag=octetstring string= asd"

}
.





oid .* 1.3.6.1.2.1.68.1.4.1.2

proc snmp {mode oid} {

	
	if {[string equal $mode next]} {
			puts "1.3.6.1.3.118.1.2.1.1.6"
		return
	}
	puts "tag=integer int32=0"
}
.

oid .* 1.3.6.1.2.1.68.1.4.1

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.68.1.4.1.2"
}
.

oid .* 1.3.6.1.2.1.68.1.4

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.68.1.4.1.2"
}
.

oid .* 1.3.6.1.2.1.68.1

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.68.1.4.1.2"
}
.

oid .* 1.3.6.1.2.1.68

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.68.1.4.1.2"
}
.



oid .* 1.3.6.1.3.118.1.2.1.1.6

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.2.1.57.0"
		return
	}

	puts "tag=integer int32=2"
}
.

oid .* 1.3.6.1.3.118.1.2.1.1

proc snmp {mode oid} {

	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3.118.1.2.1

proc snmp {mode oid} {

	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3.118.1.2

proc snmp {mode oid} {

	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3.118.1

proc snmp {mode oid} {

	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3.118

proc snmp {mode oid} {
	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3

proc snmp {mode oid} {

	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.


oid .* 1.3.6.1.2.1.4.22
proc snmp {mode oid} {


	puts "1.3.6.1.2.1.4.22.1"
}

.


oid .* 1.3.6.1.2.1.4.22.1
proc snmp {mode oid} {


	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.17.1.4.1"
		return
	}

puts "tag=octetstring string= asd"
}

.


oid .* 1.3.6.1.2.1.17.1.4.1.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
	
		puts "1.3.6.1.2.1.17.4.3.1"
		return
	}
puts "tag=octetstring string= asd"
}
.

oid .* 1.3.6.1.2.1.17.1.4

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.17.1.4.1"
}
.

oid .* 1.3.6.1.2.1.17.1

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.17.1.4.1"
}
.

oid .* 1.3.6.1.2.1.17

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.17.1.4.1"

}
.




oid .* 1.3.6.1.2.1.17.4.3.1

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set q [if_next]
	set v [nametonumber $q]
	puts "1.3.6.1.2.1.31.1.1.1.1.$v"
   
	return
}
puts "tag=octetstring string= asd"

}
.

oid .* 1.3.6.1.2.1.17.4.3

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.17.4.3.1"
}
.

oid .* 1.3.6.1.2.1.17.4

proc snmp {mode oid} {

	puts "1.3.6.1.2.1.17.4.3.1"
}
.



oid .* 1.3.6.1.2.1.31.1.1.1.1\..*

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.1.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.6.$thisn"
       }
	return
}

puts "tag=octetstring string=$interface"
}
.

oid .* 1.3.6.1.2.1.31.1.1.1.1

proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
set q [if_next]
set v [nametonumber $q]
puts "1.3.6.1.2.1.31.1.1.1.1.$v"
}
.

oid .* 1.3.6.1.2.1.31.1.1.1

proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
set q [if_next]
set v [nametonumber $q]
puts "1.3.6.1.2.1.31.1.1.1.1.$v"
}
.

oid .* 1.3.6.1.2.1.31.1.1

proc snmp {mode oid} {
	
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
set q [if_next]
set v [nametonumber $q]
puts "1.3.6.1.2.1.31.1.1.1.1.$v"

}
.

oid .* 1.3.6.1.2.1.31.1

proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
set q [if_next]
set v [nametonumber $q]
puts "1.3.6.1.2.1.31.1.1.1.1.$v"
}
.

oid .* 1.3.6.1.2.1.31

proc snmp {mode oid} {

script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl
set q [if_next]
set v [nametonumber $q]
puts "1.3.6.1.2.1.31.1.1.1.1.$v"
}
.



oid .* 1.3.6.1.2.1.31.1.1.1.6.*

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.6.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.10.$thisn"
       }
	
	return
}

set total [ exec "show interfaces total" ]

set b [ string range $total [string first $interface $total] [string length $total]]

getword
getword
set result [getword]
puts "tag=6 class=1 int64=$result"

}
.

oid .* 1.3.6.1.2.1.31.1.1.1.10.*

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.10.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.17.$thisn"
       }
	
	return
}

set total [ exec "show interfaces total" ]

set b [ string range $total [string first $interface $total] [string length $total]]

getword
getword
set result [getword]
puts "tag=6 class=1 int64=$result"

}
.




oid .* 1.3.6.1.2.1.31.1.1.1.17.*

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.17.$thisn"
       } else {
		set this [if_next ""]
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.18.$thisn"
       }
	
	return
}

puts "tag=integer int32=0"
}
.





oid .* 1.3.6.1.2.1.31.1.1.1.18.*

proc snmp {mode oid} {
script getword.tcl
script intnum.tcl
script ifname.tcl
script ifnext.tcl

if {[string equal $mode next]} {
	set this [if_next $interface]
	if {[string length $this] > 0} {
		set thisn [nametonumber $this]
		puts "1.3.6.1.2.1.31.1.1.1.18.$thisn"
       } else {
		puts "1.3.6.1.2.1.47.1.1.1.1.2"
       }
	
	return
}

set comp 0
set ma "description:"

set word ""
while { $comp < 1 }{
	set word [getword]
	set comp [string match $ma $word]
	
}

set  desc [getword]
set comp 0
set ma "type"
set comp [string match $desc $ma]
if {$comp > 0} {
	set desc ""
}

puts "tag=octetstring string=  $desc"
}
.




oid .* 1.3.6.1.3.118.1.2.1.1.6

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.3.6.3"
		return
	}


puts "tag=octetstring string=  asd"
}
.

oid .* 1.3.6.1.3.118.1.2.1.1

proc snmp {mode oid} {


puts "1.3.6.1.3.118.1.2.1.1.6"

}
.

oid .* 1.3.6.1.3.118.1.2.1

proc snmp {mode oid} {


puts "1.3.6.1.3.118.1.2.1.1.6"


}
.

oid .* 1.3.6.1.3.118.1.2

proc snmp {mode oid} {



puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3.118.1

proc snmp {mode oid} {


	puts "1.3.6.1.3.118.1.2.1.1.6"

}
.

oid .* 1.3.6.1.3.118

proc snmp {mode oid} {



	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.3

proc snmp {mode oid} {




	puts "1.3.6.1.3.118.1.2.1.1.6"
}
.

oid .* 1.3.6.1.4.1.9.3.6.6.0

script getword.tcl

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
		return
	}
set b [exec "show platform"]

set k [string first "max=" $b]
set r [ string range $b [expr $k+4] [expr $k+6]]

set result [expr $r*1024*1024]

puts "tag=integer int32=$result"
}
.

oid .* 1.3.6.1.4.1.9.3.6.6

proc snmp {mode oid} {



puts "1.3.6.1.4.1.9.3.6.6.0"


}
.



oid .* 1.3.6.1.4.1.9.3.6.3

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.3.6.6.0"
		return
	}


puts "tag=octetstring string=  itttatok"
}
.

oid .* 1.3.6.1.4.1.9.3.6

proc snmp {mode oid} {
   puts "1.3.6.1.4.1.9.3.6.3"
}
.

oid .* 1.3.6.1.4.1.9.3

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.3.6.3"
}
.
oid .* 1.3.6.1.4.1.9.2.1.58.0.*

proc snmp {mode oid} {
script getword.tcl
if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.3.6.3"
		return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
set min [getword]
puts "tag=integer class=1 int32=$min"

}
.


oid .* 1.3.6.1.4.1.9.2.1.58

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.58.0"

}
.

oid .* 1.3.6.1.4.1.9.2.1.57.0.*

proc snmp {mode oid} {

script getword.tcl
if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.2.1.58.0"
	return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
set min [getword]

puts "tag=integer class=1 int32=$min"
}
.

oid .* 1.3.6.1.4.1.9.2.1.57

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.57.0"

}
.


oid .* 1.3.6.1.4.1.9.2.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.57.0"

}
.

oid .* 1.3.6.1.4.1.9.2.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.57.0"

}
.

oid .* 1.3.6.1.4.1.9.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.57.0"

}
.

oid .* 1.3.6.1.4.1.9

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.2.1.57.0"
}
.






oid .* 1.3.6.1.4.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.2.1.57.0"

}
.

oid .* 1.3.6.1.4

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.2.1.57.0"
}
.

oid .* 1.3.6.1.4.1.9.9.48.1.1.1.6.1

proc snmp {mode oid} {

if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
	return
}
	
puts "tag=integer class=1 int32=40"	
}
.


oid .* 1.3.6.1.4.1.9.9.48.1.1.1.5.1

proc snmp {mode oid} {
script getword.tcl
if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.48.1.1.1.6.1"
	return
}
set free 5000000

	
puts "tag=integer class=1 int32=$free"	
}
.

oid .* 1.3.6.1.4.1.9.9.48.1.1.1.5

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
}
.

oid .* 1.3.6.1.4.1.9.9.48.1.1.1

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
}
.

oid .* 1.3.6.1.4.1.9.9.48.1.1

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
}
.

oid .* 1.3.6.1.4.1.9.9.48.1

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
}
.


oid .* 1.3.6.1.4.1.9.9.48

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
}
.



oid .* 1.3.6.1.4.1.9.5.1.4.1.1.9

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.5.1.4.1.1.10"
		return
	}


puts "tag=octetstring string=asd"


}
.


oid .* 1.3.6.1.4.1.9.5.1.4.1.1.10

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.5.1.4.1.1.11"
		return
	}


puts "tag=octetstring string=asd"


}
.

oid .* 1.3.6.1.4.1.9.5.1.4.1.1.11

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"
		return
	}


puts "tag=integer int32=0"


}
.




oid .* 1.3.6.1.4.1.9.5.1.4.1.1

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
}
.

oid .* 1.3.6.1.4.1.9.5.1.4.1

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
}
.

oid .* 1.3.6.1.4.1.9.5.1.4

proc snmp {mode oid} {
	puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
}
.

oid .* 1.3.6.1.4.1.9.5.1

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
}
.

oid .* 1.3.6.1.4.1.9.5

proc snmp {mode oid} {


	puts "1.3.6.1.4.1.9.5.1.4.1.1.9"
}
.

oid .* 1.3.6.1.4.1.9.9.10.1.1.2.1.2.1

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.13.1.3.1.2"
		return
	}


puts "tag=integer int32=256"

}
.


oid .* 1.3.6.1.4.1.9.9.10.1.1.2.1.2

proc snmp {mode oid} {

	


puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9.10.1.1.2.1

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9.10.1.1.2

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9.10.1.1

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9.10.1

proc snmp {mode oid} {
puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9.10

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2.1"

}
.

oid .* 1.3.6.1.4.1.9.9

proc snmp {mode oid} {


puts "1.3.6.1.4.1.9.9.10.1.1.2.1.2"

}
.





oid .* 1.3.6.1.4.1.9.9.23.1.1.1.1.2

proc snmp {mode oid} {


if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.1"
		return
	}


puts "tag=integer int32=2"



}
.

oid . * 1.3.6.1.4.1.9.9.23.1.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.23.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.23.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.23.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.23

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
}
.




oid .* 1.3.6.1.4.1.9.9.13.1.3.1.2

proc snmp {mode oid} {


if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.13.1.3.1.3"
		return
	}


puts "tag=octetstring string=enviroment"
}
.


oid .* 1.3.6.1.4.1.9.9.13.1.3.1.3

proc snmp {mode oid} {


if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.13.1.3.1.6"
		return
	}


puts "tag=octetstring string=enviroment"
}
.


oid .* 1.3.6.1.4.1.9.9.13.1.3.1.6

proc snmp {mode oid} {


if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.1.1.1.2"
		return
	}


puts "tag=octetstring string=enviroment"
}
.





oid .* 1.3.6.1.4.1.9.9.13.1.3.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.13.1.3.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.13.1.3

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.13.1.3.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.13.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.13.1.3.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.13

proc snmp {mode oid} {

puts  "1.3.6.1.4.1.9.9.13.1.3.1.2"
}
.





oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.1

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.2"
		return
	}


puts "tag=octetstring string=cdp1"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.2

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.3"
		return
	}


puts "tag=octetstring string=cdp2"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.3

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.4"
		return
	}


puts "tag=octetstring string=cdp3"

}
.



oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.4

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.5"
		return
	}


puts "tag=octetstring string=cdp4"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.5

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.6"
		return
	}


puts "tag=octetstring string=cdp5"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.6

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.7"
		return
	}


puts "tag=octetstring string=cdp6"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.7

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.8"
		return
	}


puts "tag=octetstring string=cdp7"

}
.


oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.8

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.23.1.2.1.1.9"
		return
	}


puts "tag=octetstring string=cdp8"

}
.



oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1.9

proc snmp {mode oid} {

if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.2.1.1"
		return
	}


puts "tag=octetstring string=cdp9"

}
.















oid .* 1.3.6.1.4.1.9.9.23.1.2.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.2.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.23.1.2.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.2.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.23.1.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.23.1.2.1.1.1"
}
.




oid .* 1.3.6.1.4.1.9.9.42.1.2.1.1

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.2.2.1"
		return
	}


puts "tag=octetstring string=rtrmon"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.1

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.42

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.2.1.1"
}
.



oid .* 1.3.6.1.4.1.9.9.42.1.2.2.1

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.2.5.1"
		return
	}


puts "tag=octetstring string=rtrmon"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.2

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.2.1"
}
.



oid .* 1.3.6.1.4.1.9.9.42.1.2.5.1

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.2.6.1"
		return
	}


puts "tag=octetstring string=rtrmon"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.5

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.5.1"
}
.


oid .* 1.3.6.1.4.1.9.9.42.1.2.6.1

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.2.7.1"
		return
	}


puts "tag=octetstring string=rtrmon"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.6

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.6.1"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.7.1

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.1.1.5"
		return
	}


puts "tag=octetstring string=rtrmon"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.2.7

proc snmp {mode oid} {

puts puts "1.3.6.1.4.1.9.9.42.1.2.7.1"
}
.







oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1.5

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.1.1.6"
		return
	}


puts "tag=octetstring string=capture"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1.6

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.1.1.7"
		return
	}


puts "tag=octetstring string=capture"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1.7

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.1.1.10"
		return
	}


puts "tag=octetstring string=capture"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1.10

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.1.1.11"
		return
	}


puts "tag=octetstring string=capture"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1.11

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.1"
		return
	}


puts "tag=octetstring string=capture"

}
.








oid .* 1.3.6.1.4.1.9.9.42.1.3.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.3.1.1.5"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.3.1.1.5"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.3.1.1.5"
}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.1

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.2"
		return
	}


puts "tag=octetstring string=stat"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.2

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.3"
		return
	}


puts "tag=octetstring string=stat"

}
.



oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.3

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.4"
		return
	}


puts "tag=octetstring string=stat"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.4

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.5"
		return
	}


puts "tag=octetstring string=stat"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.5

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.6"
		return
	}


puts "tag=octetstring string=stat"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.6

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.42.1.3.2.1.7"
		return
	}


puts "tag=octetstring string=stat"

}
.


oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1.7

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4""
		return
	}


puts "tag=octetstring string=stat"

}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3.2.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.3.2.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.42.1.3.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.42.1.3.2.1.1"
}
.




oid .* 1.3.6.1.4.1.9.9.46.1.3.1.1.4

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.46.1.6.1.1.14"
		return
	}


puts "tag=octetstring string=vtp"

}
.

oid .* 1.3.6.1.4.1.9.9.46.1.3.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1.3.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1.3

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4"
}
.

oid .* 1.3.6.1.4.1.9.9.46

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.3.1.1.4"
}
.







oid .* 1.3.6.1.4.1.9.9.46.1.6.1.1.14

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.48.1.1.1.5.1"
		return
	}


puts "tag=octetstring string=vtrunk"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1.6.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.6.1.1.14"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1.6.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.6.1.1.14"
}
.

oid .* 1.3.6.1.4.1.9.9.46.1.6

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.46.1.6.1.1.14"
}
.









oid .* 1.3.6.1.4.1.9.9.68.1.2.2.1.2

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.68.1.5.1.1.1"
		return
	}


puts "tag=octetstring string=wm"


}
.

oid .* 1.3.6.1.4.1.9.9.68.1.2.2.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1.2.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.68

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.2.2.1.2"
}
.



oid .* 1.3.6.1.4.1.9.9.68.1.5.1.1.1

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.106.1.2.1.1.11"
		return
	}


puts "tag=octetstring string=wmvoice"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1.5.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.5.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1.5.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.5.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.68.1.5

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.68.1.5.1.1.1"
}
.











oid .* 1.3.6.1.4.1.9.9.106.1.2.1.1.11

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2"
		return
	}


puts "tag=octetstring string=hsrp"
}
.

oid .* 1.3.6.1.4.1.9.9.106.1.2.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.106.1.2.1.1.11"
}
.

oid .* 1.3.6.1.4.1.9.9.106.1.2.1

proc snmp {mode oid} {

puts 
}
.

oid .* 1.3.6.1.4.1.9.9.106.1.2

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.106.1.2.1.1.11"
}
.

oid .* 1.3.6.1.4.1.9.9.106.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.106.1.2.1.1.11"
.

oid .* 1.3.6.1.4.1.9.9.106

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.106.1.2.1.1.11"
}
.


oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1.2.*

proc snmp {mode oid} {


	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.109.1.1.1.1.4.1"
		return
	}


puts "tag=octetstring string=cpu"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1.4

proc snmp {mode oid} {
script getword.tcl


if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.109.1.1.1.1.4.1"
	return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
set min [getword]

puts "tag=integer class=1 int32=$min"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1.4.1.*

proc snmp {mode oid} {
script getword.tcl


if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.109.1.1.1.1.5.1"
	return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
set min [getword]

puts "tag=integer class=1 int32=$min"
}
.


oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1.5

proc snmp {mode oid} {
script getword.tcl

if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.109.1.1.1.1.5.1"
	return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
set min [getword]
puts "tag=integer class=1 int32=$min"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1.5.1.*

proc snmp {mode oid} {
script getword.tcl

if {[string equal $mode next]} {
	puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
	return
}
set b [ exec "show system" ]
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
getword
set min [getword]
puts "tag=integer class=1 int32=$min"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.109.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2" 
}
.

oid .* 1.3.6.1.4.1.9.9.109.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.109

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.109.1.1.1.1.2"
}
.











oid .* 1.3.6.1.4.1.9.9.166.1.1.1.1.2.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.1.1.1.3"
		return
	}


puts "tag=octetstring string=c"


}
.


oid .* 1.3.6.1.4.1.9.9.166.1.1.1.1.3.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.1.1.1.4"
		return
	}


puts "tag=octetstring string=c"


}
.

oid .* 1.3.6.1.4.1.9.9.166.1.1.1.1.4.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.1.1.1.9"
		return
	}


puts "tag=octetstring string=c"


}
.

oid .* 1.3.6.1.4.1.9.9.166.1.1.1.1.9.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.5.1.1.2"
		return
	}


puts "tag=octetstring string=c"


}
.


oid .* 1.3.6.1.4.1.9.9.166.1.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.1.1.1.2"
}
.






oid .* 1.3.6.1.4.1.9.9.166.1.5.1.1.2.*

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.5.1.1.3"
		return
	}


puts "tag=octetstring string=h"


}
.

oid .* 1.3.6.1.4.1.9.9.166.1.5.1.1.3.*

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.5.1.1.4"
		return
	}


puts "tag=octetstring string=h"


}
.


oid .* 1.3.6.1.4.1.9.9.166.1.5.1.1.4.*

proc snmp {mode oid} {

	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.6.1.1.1"
		return
	}


puts "tag=octetstring string=h"


}
.




oid .* 1.3.6.1.4.1.9.9.166.1.5.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.5.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.5.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.5.1.1.2"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.5

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.5.1.1.2"
}
.







oid .* 1.3.6.1.4.1.9.9.166.1.6.1.1.1.*

proc snmp {mode oid} {
	if {[string equal $mode next]} {
			puts "1.3.6.1.4.1.9.9.166.1.7.1.1.1"
		return
	}


puts "tag=octetstring string=mandjemvege"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.6.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.6.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.6.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.6.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.6

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.6.1.1.1"
}
.






oid .* 1.3.6.1.4.1.9.9.166.1.7.1.1.1.*

proc snmp {mode oid} {

	if {[string equal $mode next]} {
		puts "1.3.6.1.4.1.9.9.166.1.7.1.1.1"	
		return
	}

puts "tag=octetstring string=vege"

}
.

oid.* 1.3.6.1.4.1.9.9.166.1.7.1.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.7.1.1.1"
}
.

oid.* 1.3.6.1.4.1.9.9.166.1.7.1

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.7.1.1.1"
}
.

oid .* 1.3.6.1.4.1.9.9.166.1.7

proc snmp {mode oid} {

puts "1.3.6.1.4.1.9.9.166.1.7.1.1.1"
}
.


