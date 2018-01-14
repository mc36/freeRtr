proc trafficMultiplierA { hops } {
for {set a 1} {$a <= $hops} {incr a} {
  set c [expr $a/256]
  set d [expr $a%256]
  puts "interface loopback$a"
  puts " vrf for v"
  puts " ipv4 addr 12.3.$c.$d 255.255.255.255"
  puts " exit"
  }
}

proc trafficMultiplierB { hops } {
puts "router bgp4 1"
puts " vrf v"
for {set a 1} {$a <= $hops} {incr a} {
  set c [expr $a/256]
  set d [expr $a%256]
  puts " neighbor 1.9.$c.$d remote-as 1"
  puts " neighbor 1.9.$c.$d address-family unicast"
  puts " neighbor 1.9.$c.$d update-source loopback$a"
  puts " neighbor 1.9.$c.$d route-policy-in v"
  }
puts " exit"
}

proc trafficMultiplier { hops } {
trafficMultiplierA $hops
trafficMultiplierB $hops
}

trafficMultiplier 10
