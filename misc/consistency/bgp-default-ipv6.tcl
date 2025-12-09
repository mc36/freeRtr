set a [exec "show ipv6 bgp 1955 unicast allroute ::/0 | include update"]
puts "$a"
