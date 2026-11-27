set m 4000

for {set a 1} {$a <= $m} {incr a} {
  set b [expr ($a*10) + 1]
  set c [expr ($a*10) + 2]
  config "hairpin $a"
  config "vrf definition mokus$a"
  config "interface dialer$a" "encapsulation ppp"
  config "interface dialer$a" "ppp ip4cp open"
  config "interface dialer$a" "ppp ip4cp local 0.0.0.0"
  config "interface dialer$a" "ppp ip6cp open"
  config "interface dialer$a" "ppp username xxx"
  config "interface dialer$a" "ppp password xxx"
  config "interface dialer$a" "vrf forwarding mokus$a"
  config "interface dialer$a" "ipv4 address dynamic 0.0.0.0"
  config "interface dialer$a" "ipv6 address dynamic 0.0.0.0"
  config "interface hairpin$b" "p2poe client dialer$a"
  config "interface hairpin$c" "bridge-group 1"
}


for {set a 1} {$a <= $m} {incr a} {
  puts [exec "ping 94.21.255.255 vrf mokus$a"]
}
