set m 4000

for {set a 1} {$a <= $m} {incr a} {
  set b [expr $a/256]
  set c [expr $a%256]
  config "vrf definition main$a"
  config "vrf definition suli$a" "rd 6500:1025"
  config "vrf definition suli$a" "rt-both 1955:6661666"
  config "interface loopback$a" "vrf forwarding suli$a"
  config "interface loopback$a" "ipv4 address 10.241.$b.$c 255.255.255.255"
  config "interface virtualppp$a" "encapsulation ppp"
  config "interface virtualppp$a" "ppp ip4cp open"
  config "interface virtualppp$a" "ppp ip4cp local 100.64.$b.$c"
  config "interface virtualppp$a" "ppp mplscp open"
  config "interface virtualppp$a" "vrf forwarding main$a"
  config "interface virtualppp$a" "ipv4 address dynamic 255.255.255.255"
  config "interface virtualppp$a" "mpls enable"
  config "interface virtualppp$a" "pseudowire v1 loopback0 l2tp2 195.111.97.12 1234"
  config "router bgp4 $a" "vrf main$a"
  config "router bgp4 $a" "local-as 65000"
  config "router bgp4 $a" "router-id 10.45.$b.$c"
  config "router bgp4 $a" "no safe-ebgp"
  config "router bgp4 $a" "neighbor 10.7.255.255 remote-as 1955"
  config "router bgp4 $a" "neighbor 10.7.255.255 address-family vpnuni ovpnuni vpls"
  config "router bgp4 $a" "neighbor 10.7.255.255 send-community all"
  config "router bgp4 $a" "neighbor 195.111.94.251 remote-as 1955"
  config "router bgp4 $a" "neighbor 195.111.94.251 address-family labeled"
  config "router bgp4 $a" "afi-vrf suli$a enable"
  config "router bgp4 $a" "afi-vrf suli$a redistribute connected"
  config "router bgp4 $a" "justadvert virtualppp$a"
}


for {set a 1} {$a <= $m} {incr a} {
  puts [exec "ping 8.8.8.8 vrf suli$a"]
}
