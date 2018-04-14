proc makeConfig { x port } {
puts $x "addremote r1"
puts $x "int eth1 eth 0000.0000.2222 127.0.0.1 1234 127.0.0.1 1235"
puts $x "!"
puts $x "config term"
puts $x "vrf definition v1"
puts $x " rd 1:1"
puts $x " exit"
puts $x "vrf definition v2"
puts $x " rd 1:2"
puts $x " exit"
puts $x "interface tunnel11"
puts $x " tunnel key $port"
puts $x " tunnel vrf sulinet"
puts $x " tunnel source loopback2"
puts $x " tunnel destination 193.224.22.9"
puts $x " tunnel mode pckoip"
puts $x " vrf forwarding v1"
puts $x " ipv4 address 1.1.1.9 255.255.255.252"
puts $x " exit"
puts $x "interface tunnel12"
puts $x " tunnel key $port"
puts $x " tunnel vrf uplink"
puts $x " tunnel source ethernet1"
puts $x " tunnel destination 195.199.255.209"
puts $x " tunnel mode pckoip"
puts $x " vrf forwarding v2"
puts $x " ipv4 address 1.1.1.10 255.255.255.252"
puts $x " exit"
puts $x "end"
puts $x "!"
}


proc testCaseA2B { port } {
set x [open port-ip-a2b-$port.tst w]
puts $x "description ip inet2suli $port"
makeConfig $x $port
puts $x "r1 tping 100 10 1.1.1.9 /vrf v2"
close $x
}

proc testCaseB2A { port } {
set x [open port-ip-b2a-$port.tst w]
puts $x "description ip suli2inet $port"
makeConfig $x $port
puts $x "r1 tping 100 10 1.1.1.10 /vrf v1"
close $x
}

for {set a 1} {$a < 255} {incr a} {
  testCaseA2B $a
  testCaseB2A $a
  }
