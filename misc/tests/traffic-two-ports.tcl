proc trafficMultiplierA { hops } {
for {set a 1} {$a <= $hops} {incr a} {
  set b [expr $a+100]
  set c [expr (($a+2)/2)+100]
  set d "sdn8.$b"
  set e "inter $d"
  set f "router lsrp4 $c"
  config "vrf def big$c" "rd $b:$c"
  config "$f" "vrf big$c"
  config "$f" "router-id 253.252.$a.1"
  config "$f" "red conn"
  config "$e" "vrf for big$c"
  config "$e" "ipv4 addr 253.252.$a.1 /30"
  config "$e" "$f ena"
  config "serv p4la p4" "export-vrf big$c $c"
  }
}

proc trafficMultiplierB { hops } {
for {set a 1} {$a <= $hops} {incr a} {
  set b [expr $a+100]
  set c [expr (($a+1)/2)+500]
  set d "sdn9.$b"
  set e "inter $d"
  set f "router lsrp4 $c"
  config "vrf def big$c" "rd $b:$c"
  config "$f" "vrf big$c"
  config "$f" "router-id 253.252.$a.2"
  config "$f" "red conn"
  config "$e" "vrf for big$c"
  config "$e" "ipv4 addr 253.252.$a.2 /30"
  config "$e" "$f ena"
  config "serv p4la p4" "export-vrf big$c $c"
  }
}

proc trafficMultiplier { hops } {
trafficMultiplierA $hops
trafficMultiplierB $hops
}


trafficMultiplierA 10
trafficMultiplierB 10
