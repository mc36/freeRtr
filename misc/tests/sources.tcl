for {set a 0} {$a <= 100} {incr a} {
  config "inter lo666" "vrf for inet2"
  config "inter lo666" "ipv6 addr 2001:4c4e:8:1bae:59:5150:2926:$a /128"
  puts [exec "ping vpn.nop.hu /vrf inet2 /int lo666"]
  }
