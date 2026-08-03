proc configureBGP { hops } {
for {set a 1} {$a <= $hops} {incr a} {
  set c [expr $a/256]
  set d [expr $a%256]
  puts "bgp$a"
  config "hair $a" "no ether"
  config "int hair$a1" "enc raw" "vrf for v1" "ipv4 addr 1.$c.$d.1 255.255.255.252"
  config "int hair$a2" "enc raw" "vrf for v2" "ipv4 addr 1.$c.$d.2 255.255.255.252"
  config "router bgp4 2" "neigh 1.$c.$d.1 temp rr"
  }
}

proc configureLO { hops } {
for {set a 1} {$a <= $hops} {incr a} {
  puts "lo$a"
  set b [expr $a/65536]
  set c [expr $a%65536]
  set d [expr $c/256]
  set e [expr $c%256]
  config "int lo$a" "vrf for v3" "ipv4 addr 2.$b.$d.$e 255.255.255.255"
  }
}

configureBGP 10
configureLO 10
