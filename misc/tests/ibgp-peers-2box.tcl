proc configureHW1 { hops } {
set x [open r1-hw.txt w]
puts $x "nostall"
for {set a 1} {$a <= $hops} {incr a} {
  set e [expr ($a*2)+25000]
  set f [expr $e+1]
  puts $x "int serial$a ser - 127.0.0.1 $e 127.0.0.1 $f"
  }
}

proc configureHW2 { hops } {
set x [open r2-hw.txt w]
puts $x "nostall"
for {set a 1} {$a <= $hops} {incr a} {
  set e [expr ($a*2)+25000]
  set f [expr $e+1]
  puts $x "int serial$a ser - 127.0.0.1 $f 127.0.0.1 $e"
  }
close $x
}



proc configureSW1 { hops } {
set x [open r1-sw.txt w]
puts $x "logging file debug r1-log.txt"
puts $x "vrf def v1"
puts $x " exit"
puts $x "router bgp4 1"
puts $x " vrf v1"
puts $x " local-as 1"
puts $x " exit"
for {set a 1} {$a <= $hops} {incr a} {
  set c [expr $a/256]
  set d [expr $a%256]
  puts $x "int ser$a"
  puts $x " enc raw"
  puts $x " vrf for v1"
  puts $x " ipv4 addr 1.$c.$d.1 255.255.255.252"
  puts $x " exit"
  puts $x "router bgp4 1"
  puts $x " neigh 1.$c.$d.2 remote-as 1"
  puts $x " exit"
  }
close $x
}


proc configureSW2 { hops } {
set x [open r2-sw.txt w]
puts $x "logging file debug r2-log.txt"
puts $x "vrf def v1"
puts $x " exit"
puts $x "router bgp4 1"
puts $x " vrf v1"
puts $x " local-as 1"
puts $x " exit"
for {set a 1} {$a <= $hops} {incr a} {
  set c [expr $a/256]
  set d [expr $a%256]
  puts $x "int ser$a"
  puts $x " enc raw"
  puts $x " vrf for v1"
  puts $x " ipv4 addr 1.$c.$d.2 255.255.255.252"
  puts $x " exit"
  puts $x "router bgp4 1"
  puts $x " neigh 1.$c.$d.1 remote-as 1"
  puts $x " exit"
  }
close $x
}



set lim 10
configureHW1 $lim
configureHW2 $lim
configureSW1 $lim
configureSW2 $lim
