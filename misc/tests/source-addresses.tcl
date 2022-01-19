for {set a 0} {$a <= 0x100} {incr a} {
  config "inter lo666" "vrf for inet2"
  config "inter lo666" "ipv6 addr 2001:738:2001:db8::[string tohex w $a] /128"
  puts [exec "ping vpn.nop.hu /vrf inet2 /int lo666"]
  }
