set a [exec "show ipv4 bgp 1955 unicast allroute 0.0.0.0/0 | inc preference"]
puts "$a"
