proc testCaseA2B { port } {
set x [open port-tcp-a2b-$port.tst w]
puts $x "description tcp inet2suli $port"
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
puts $x "interface dialer11"
puts $x " encapsulation raw"
puts $x " vrf forwarding v1"
puts $x " ipv4 address 1.1.1.5 255.255.255.252"
puts $x " exit"
puts $x "interface dialer12"
puts $x " encapsulation raw"
puts $x " vrf forwarding v2"
puts $x " ipv4 address 1.1.1.6 255.255.255.252"
puts $x " exit"
puts $x "server pckotcp a"
puts $x " port $port"
puts $x " clone dialer11"
puts $x " vrf sulinet"
puts $x " exit"
puts $x "vpdn a"
puts $x " interface dialer12"
puts $x " proxy uplink"
puts $x " target 195.199.255.209"
puts $x " vcid $port"
puts $x " protocol pckotcp"
puts $x " exit"
puts $x "end"
puts $x "!"
puts $x "r1 tping 100 10 1.1.1.5 vrf v2"
close $x
}

proc testCaseB2A { port } {
set x [open port-tcp-b2a-$port.tst w]
puts $x "description tcp suli2inet $port"
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
puts $x "interface dialer11"
puts $x " encapsulation raw"
puts $x " vrf forwarding v1"
puts $x " ipv4 address 1.1.1.5 255.255.255.252"
puts $x " exit"
puts $x "interface dialer12"
puts $x " encapsulation raw"
puts $x " vrf forwarding v2"
puts $x " ipv4 address 1.1.1.6 255.255.255.252"
puts $x " exit"
puts $x "server pckotcp a"
puts $x " port $port"
puts $x " clone dialer11"
puts $x " vrf uplink"
puts $x " exit"
puts $x "vpdn a"
puts $x " interface dialer12"
puts $x " proxy sulinet"
puts $x " target 193.224.22.9"
puts $x " vcid $port"
puts $x " protocol pckotcp"
puts $x " exit"
puts $x "end"
puts $x "!"
puts $x "r1 tping 100 10 1.1.1.6 vrf v1"
close $x
}

for {set a 1} {$a < 65535} {incr a} {
  testCaseA2B $a
  testCaseB2A $a
  }
