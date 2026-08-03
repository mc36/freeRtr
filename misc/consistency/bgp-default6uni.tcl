set a [exec "show ipv6 bgp 1955 unicast allroute ::/0 | inc preference"]
puts "$a"
